/* Copyright 2018 <samuelrivas@gmail.com

   This works with reverse polish notation to avoid dealing with parenthesis,
   but we could get them back just by translating.

   A proper solution should use a graph of states to avoid copying over the
   partial solution and the figure stack for every tested permutation.

   This will not work for long sequences of numbers but is enough for 8 or so.
 */
#include <vector>
#include <queue>
#include <stack>
#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

using std::vector;
using std::queue;
using std::stack;
using std::cout;
using std::endl;
using std::ostringstream;
using std::string;

enum class Type { Operator, Value };
enum class Operator { Sum, Minus, Times, Div };

vector<Operator> operators {
  Operator::Sum,
    Operator::Minus,
    Operator::Times,
    Operator::Div
};

double operate(double a, double b, const Operator& op) {
  switch (op) {
    case Operator::Sum:
      return a + b;
    case Operator::Minus:
      return a - b;
    case Operator::Times:
      return a * b;
    case Operator::Div:
      return a / b;
  }
  assert(false);
}

struct Element {
  Type type;
  union {
    Operator op;
    double value;
  };
 public:
  explicit Element(Operator _op) :
    type { Type::Operator },
    op { _op } { }
  explicit Element(double _value) :
    type { Type::Value },
    value { _value } { }
};

string operator_to_string(const Operator& op) {
  ostringstream out;
  switch (op) {
  case Operator::Sum:
    out << "+";
    break;
  case Operator::Minus:
    out << "-";
    break;
  case Operator::Times:
    out << "*";
    break;
  case Operator::Div:
    out << "/";
    break;
  }
  return out.str();
}

string element_to_string(const Element& element) {
  if (element.type == Type::Value) {
    ostringstream out;
    out << element.value;
    return out.str();
  } else {
    return operator_to_string(element.op);
  }
}

string solution_to_string(const vector<Element>& solution) {
  ostringstream out;

  for (Element x : solution) {
    out << element_to_string(x) << " ";
  }
  return out.str();
}

void guess_arithmetic_rec(queue<double> figures, double goal, stack<double> st,
                          vector<Element> partial_solution,
                          vector<vector<Element>>* solutions) {
  if (figures.empty() && st.size() == 1) {
    if (st.top() == goal) {
      solutions -> push_back(partial_solution);
    }
    return;
  }

  if (!figures.empty()) {
    stack<double> new_stack(st);
    queue<double> new_figures(figures);
    vector<Element> new_partial_solution(partial_solution);

    double figure = new_figures.front();
    new_figures.pop();
    Element element(figure);
    new_stack.push(figure);
    new_partial_solution.push_back(element);

    guess_arithmetic_rec(new_figures, goal, new_stack, new_partial_solution,
                         solutions);
  }

  if (st.size() > 1) {
    vector<Element> new_partial_solution(partial_solution);

    double b = st.top();
    st.pop();
    double a = st.top();
    st.pop();

    for (Operator op : operators) {
      if (op == Operator::Div && b == 0) {
        continue;
      }

      stack<double> new_stack(st);
      vector<Element> new_partial_solution(partial_solution);

      Element element(op);
      double result = operate(a, b, op);
      new_stack.push(result);
      new_partial_solution.push_back(element);

      guess_arithmetic_rec(figures, goal, new_stack, new_partial_solution,
                           solutions);
    }
  }
}

vector<vector<Element>> guess_arithmetic(const queue<double>& figures,
                                         double goal) {
  vector<vector<Element>> solutions;
  stack<double> st;
  vector<Element> partials;

  guess_arithmetic_rec(figures, goal, st, partials, &solutions);

  return solutions;
}

int main(void) {
  queue<double> test;
  test.push(3);
  test.push(1);
  test.push(3);
  test.push(6);

  for (auto solution : guess_arithmetic(test, 8)) {
    cout << solution_to_string(solution) << endl;
  }

  return 0;
}
