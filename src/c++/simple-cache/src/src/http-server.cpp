#include <Poco/Net/ServerSocket.h>
#include <Poco/Net/HTTPServer.h>
#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPResponse.h>
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Util/ServerApplication.h>
#include <iostream>
#include <string>
#include <vector>

using std::cerr;
using std::endl;
using std::ostream;
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

class ScacheRequestHandler : public HTTPRequestHandler {
 public:
  virtual void handleRequest(HTTPServerRequest &req, HTTPServerResponse &resp) {
    resp.setStatus(HTTPResponse::HTTP_OK);
    resp.setContentType("text/html");

    ostream& out = resp.send();
    out << "<h1>Hello world!</h1>"
        << "<p>Count: "  << ++count         << "</p>"
        << "<p>Host: "   << req.getHost()   << "</p>"
        << "<p>Method: " << req.getMethod() << "</p>"
        << "<p>URI: "    << req.getURI()    << "</p>";
    out.flush();

    cerr << "Response sent for count=" << count
         << " and URI=" << req.getURI() << endl;
  }

 private:
  static int count;
};

int ScacheRequestHandler::count = 0;

class ScacheRequestHandlerFactory : public HTTPRequestHandlerFactory {
 public:
  virtual HTTPRequestHandler* createRequestHandler(const HTTPServerRequest &) {
    return new ScacheRequestHandler;
  }
};

class ScacheServerApp : public ServerApplication {
 protected:
  int main(const vector<string> &) {
    HTTPServer s(new ScacheRequestHandlerFactory, ServerSocket(9090), new HTTPServerParams);

    s.start();
    cerr << "Scache server started" << endl;

    waitForTerminationRequest();  // wait for CTRL-C or kill

    cerr << "Scache server shutting down" << endl;
    s.stop();

    return Application::EXIT_OK;
  }
};

int main(int argc, char* argv[]) {
  ScacheServerApp app;
  return app.run(argc, argv);
}
