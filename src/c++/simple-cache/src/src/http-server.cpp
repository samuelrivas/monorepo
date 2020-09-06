/* Copyright 2020 samuelrivas@gmail.com
 *
 * Simple HTTP server to implement in memory key value store.
 *
 * POSTing to /key, stores the body as value for key
 *
 * GETting /key, returns the value for key if it was previously stored, or 404
 * if not.
 *
 * Values can be modified by posting to an existing key.
 *
 * Pending:
 *
 * TODO(Samuel) Expire keys. We need to store the current system type as age
 * when writing a new value, and expire all keys stored earlier than age - 30
 * minutes. We also need to filter out old values when getting.
 *
 * TODO(Samuel) Logging needs to lock the output stream. I am not doing it now
 * for simplicity, but with load the log gets garbled.
 */
#include <Poco/Net/ServerSocket.h>
#include <Poco/Net/HTTPServer.h>
#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Util/ServerApplication.h>
#include <Poco/URI.h>
#include <Poco/ErrorHandler.h>

#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <optional>
#include <utility>

#include "scache.hpp"
#include "shared-cache.hpp"

using std::cerr;
using std::endl;
using std::optional;
using std::ostream;
using std::move;
using std::nullopt;
using std::string;
using std::vector;
using std::istream;
using std::byte;

using Poco::Net::HTTPRequestHandler;
using Poco::Net::HTTPRequestHandlerFactory;
using Poco::Net::HTTPResponse;
using Poco::Net::HTTPServer;
using Poco::Net::HTTPServerParams;
using Poco::Net::HTTPServerRequest;
using Poco::Net::HTTPServerResponse;
using Poco::Net::ServerSocket;
using Poco::Util::ServerApplication;
using Poco::URI;
using Poco::ErrorHandler;
using Poco::Exception;

using K = string;
using V = vector<byte>;
using Timestamp = typename sam::SCache<K, V>::Timestamp;
using Cache = sam::SharedCache<K, V>;

enum class Method {
  Get,
  Post
};

// The default error handler silently fails. This one is not awesome, but better
// than nothing
class ScacheErrorHandler : public ErrorHandler {
 public:
  virtual void exception(const Exception& x) {
    (void) x;
    cerr << "Uncaught exception" << x.displayText() << endl;
  }
  virtual void exception(const std::exception& x) {
    (void) x;
    cerr << "Uncaught exception: " << x.what() << endl;
  }
  virtual void exception() {
    cerr << "Uncaught exception: unknown" << endl;
  }
};

class ScacheRequestHandler : public HTTPRequestHandler {
 private:
  Cache* cache;

 public:
  ScacheRequestHandler(Cache* _cache) :
    cache { _cache }
  {};

  // Handlers called by this may, but don't need to, send the response. If they
  // don't rend the response, this function will send it with an empty body.
  virtual void handleRequest(HTTPServerRequest &req, HTTPServerResponse &resp) {

    optional<Method> method = parseMethod(req);
    optional<string> key = parseKey(req);

    if (! method.has_value()) {
      resp.setStatusAndReason(HTTPResponse::HTTP_METHOD_NOT_ALLOWED);
      finishRequest(resp);
      return;
    }

    if (! key.has_value()) {
      resp.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST,
                              "Path must have a single segment");
      finishRequest(resp);
      return;
    }

    switch (method.value()) {
    case Method::Get:
      handleGet(key.value(), req, resp, cache);
      break;
    case Method::Post:
      handlePost(key.value(), req, resp, cache);
      break;
    }

    if (! resp.sent()) {
      resp.send().flush();
    }
  }

 private:
  void handlePost(const string& key,
                  HTTPServerRequest &req,
                  HTTPServerResponse &resp,
                  Cache* cache) {

    optional<vector<byte>> body = readBody(req);

    if (! body.has_value()) {
      resp.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST,
                              "You must send a body when POSTing");
    } else {
      resp.setStatus(HTTPResponse::HTTP_OK);
      cerr << "New value for '" << key << "', "
           << body.value().size() << " bytes" << endl;
      cache -> insert(key, move(body.value()), 0);
    }
  }

  void handleGet(const string& key,
                 HTTPServerRequest &req,
                 HTTPServerResponse &resp,
                 Cache *cache) {
    (void) req;

    optional<pair<const V&, Timestamp>> optional_value = cache -> lookup(key);
    cerr << "Lookup '" << key << "'" << endl;

    if (optional_value.has_value()) {
      const V& value = optional_value.value().first;
      size_t length = value.size();

      cerr << "Found " << length << " bytes value for '"
           << key << "'" << endl;

      resp.setStatusAndReason(HTTPResponse::HTTP_OK);
      resp.setContentType("application/octet-stream");
      ostream& out = resp.send();

      out.write(reinterpret_cast<const char*>(value.data()), length);
      out.flush();
    } else {
      cerr << "Not found '" << key << "'" << endl;
      resp.setStatusAndReason(HTTPResponse::HTTP_NOT_FOUND);
      finishRequest(resp);
    }
  }

  optional<Method> parseMethod(const HTTPServerRequest &req) const {
    string method = req.getMethod();
    if (method == "GET") {
      return Method::Get;
    } else if (method == "POST") {
      return Method::Post;
    } else {
      return nullopt;
    }
  }

  optional<vector<byte>> readBody(HTTPServerRequest &req) {
    int length = req.getContentLength();

    if (length < 0) {
      return nullopt;
    }

    //TODO(Samuel) add a limit here
    vector<byte> bytes(length);
    req.stream().read(reinterpret_cast<char*>(bytes.data()), length);

    //Just in case something failed
    if (bytes.size() != static_cast<size_t>(length)) {
      cerr << "ERROR: could not read " << length
           << " bytes from request body" << endl;
      return nullopt;
    }
    cerr << "Read " << length << " bytes from request body" << endl;
    return bytes;
  }

  // If the request path is of type `/keyname`, return `keyname`. Otherwise
  // return an empty optional
  optional<string> parseKey(const HTTPServerRequest &req) const {
    vector<string> path;
    URI(req.getURI()).getPathSegments(path);

    if (path.size() == 1) {
      return path[0];
    } else {
      return nullopt;
    }
  }

  void finishRequest(HTTPServerResponse &resp) {
    resp.send().flush();
  }
};

class ScacheRequestHandlerFactory : public HTTPRequestHandlerFactory {
 private:
  Cache* cache;
 public:
  ScacheRequestHandlerFactory(Cache* _cache) :
    cache { _cache }
  {};

  virtual HTTPRequestHandler* createRequestHandler(const HTTPServerRequest &) {
    return new ScacheRequestHandler(cache);
  }
};

class ScacheServerApp : public ServerApplication {
 private:
  Cache cache;
 protected:
  int main(const vector<string> &) {
    HTTPServer server(new ScacheRequestHandlerFactory(&cache),
                      ServerSocket(8080),
                      new HTTPServerParams);

    server.start();
    cerr << "Scache server started" << endl;
    waitForTerminationRequest();
    cerr << "Scache server shutting down" << endl;
    server.stop();

    return Application::EXIT_OK;
  }
};

int main(int argc, char* argv[]) {
  ScacheServerApp app;
  ScacheErrorHandler errorHandler;
  Poco::ErrorHandler::set(&errorHandler);

  return app.run(argc, argv);
}
