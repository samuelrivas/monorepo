// https://www.careercup.com/question?id=5630954857562112

#include <vector>
#include <iostream>
#include <unordered_map>
#include <utility>
#include <string>

#include "lib/graph.hpp"

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::unordered_map;
using std::pair;

typedef pair<string,vector<string>> Contact;

pair<unordered_map<string, int>, unordered_map<int, string>>
make_email_indices(const vector<Contact>& contacts) {
  unordered_map<string, int> mail_to_id;
  unordered_map<int, string> id_to_mail;
  int fresh_id = 0;

  for (Contact contact : contacts) {
    for (string mail : contact.second) {
      if (mail_to_id.find(mail) == mail_to_id.end()) {
        id_to_mail[fresh_id] = mail;
        mail_to_id[mail] = fresh_id;
        fresh_id++;
      } else {
        cerr << "Found duplicate email when building index: " << mail << endl;
      }
    }
  }

  return { mail_to_id, id_to_mail };
}

Graph build_mail_graph(const vector<Contact>& contacts,
                       const unordered_map<string, int>& mail_to_id) {
  Graph mail_graph(mail_to_id.size());

  for (Contact contact : contacts) {
    int first_id = mail_to_id.find(contact.second[0]) -> second;
    for (size_t i = 1; i < contact.second.size(); i++) {
      mail_graph.connect(first_id,
                         mail_to_id.find(contact.second[i]) -> second);
    }
  }
  return mail_graph;
}

class MergeCallbacks : public BfsCallbacks {
  vector<int> connected;

public:
  void on_entry(int vertex,
                const vector<int>& parent,
                const vector<State>& state) override {
    (void) parent;
    (void) state;
    connected.push_back(vertex);
  }

  vector<int> get_connected() {
    return connected;
  }
};

vector<int> get_connected(const Graph& mail_graph, int mail_id) {
  MergeCallbacks cb;
  Bfs bfs(mail_graph, &cb);
  bfs.bfs(mail_id);
  return cb.get_connected();
}

vector<Contact> merge_contacts(const vector<Contact>& contacts) {

  // Our graph library uses integers from 0 to N as node ids, we thus need to
  // map our emails to that range so that we can use it
  pair<unordered_map<string, int>, unordered_map<int, string>> mail_indices =
    make_email_indices(contacts);

  // We now build a graph where nodes are emails, and where all the emails of a
  // user are connected
  Graph mail_graph = build_mail_graph(contacts, mail_indices.first);

  // We now find all connected components, which are the resulting merged users
  vector<Contact> merged_contacts;
  vector<string> email_user_id(mail_indices.first.size());
  for (Contact contact : contacts) {
    string first_mail = contact.second[0];
    int first_mail_id = mail_indices.first[first_mail];

    if (email_user_id[first_mail_id] == "") {
      // We haven't seen this email yet (and none of it's connected emails
      // either). We thus add the user to the merged users list, with all the
      // emails reachable from this one
      cerr << "Adding " << contact.first << endl;
      vector<int> connected_mids = get_connected(mail_graph, first_mail_id);
      Contact new_contact { contact.first, {} };
      for (int mid : connected_mids) {
        email_user_id[mid] = contact.first;
        string user_mail = mail_indices.second[mid];
        cerr << "  " << user_mail << endl;
        new_contact.second.push_back(user_mail);
      }
      merged_contacts.push_back(new_contact);
    } else {
      // This user has an email that is connected to another user, we can just
      // ignore it
      cerr << "User " << contact.first << " merged with "
           << email_user_id[first_mail_id] << endl;
    }
  }
  return merged_contacts;
}

int main(void) {
  vector<Contact> test {
    { "1", { "a", "b", "c" } },
    { "2", { "d", "b", "f" } },
    { "3", { "g", "d", "i" } },
    { "4", { "x", "y" } },
    { "5", { "h", "m", "n", "l" } },
    { "6", { "n" } }
  };

  vector<Contact> merged = merge_contacts(test);

  for (Contact contact : merged) {
    cout << contact.first << ": ";
    string separator = "";

    for (string mail : contact.second) {
      cout << separator << mail;
      separator = ", ";
    }
    cout << endl;
  }
  return 0;
}
