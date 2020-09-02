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

using std::cerr;
using std::endl;
using std::optional;
using std::ostream;
using std::nullopt;
using std::string;
using std::vector;

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
 public:
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
      handleGet(req, resp);
      break;
    case Method::Post:
      handlePost(req, resp);
      break;
    }

    // TODO(Samuel) Remove this
    ostream& out = resp.send();
    out << "<h1>Hello world!</h1>"
        << "<p>Host: "   << req.getHost()   << "</p>"
        << "<p>Method: " << req.getMethod() << "</p>"
        << "<p>URI: "    << req.getURI()    << "</p>";
    out.flush();

    cerr << "Response sent for URI=" << req.getURI() << endl;

  }

 private:
  void handlePost(HTTPServerRequest &req, HTTPServerResponse &resp) {
    vector<string> path;
    URI(req.getURI()).getPathSegments(path);

    optional<string> key = parseKey(req);
    cerr << "optional has " << key.value_or("EMPTY") << endl;

    if (path.size() != 1) {
      resp.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST,
                              "Path must have a single segment");
      return;
    }

    cerr << "Store key " << path[0] << endl;

    resp.setStatus(HTTPResponse::HTTP_OK);
    resp.setContentType("application/octet-stream");
  }

  void handleGet(HTTPServerRequest &req, HTTPServerResponse &resp) {
    (void) req;
    cerr << "Handling get" << endl;
    resp.setStatus(HTTPResponse::HTTP_OK);
    resp.setContentType("application/octet-stream");
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
 public:
  virtual HTTPRequestHandler* createRequestHandler(const HTTPServerRequest &) {
    return new ScacheRequestHandler;
  }
};

class ScacheServerApp : public ServerApplication {
 protected:
  int main(const vector<string> &) {
    HTTPServer server(new ScacheRequestHandlerFactory,
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
