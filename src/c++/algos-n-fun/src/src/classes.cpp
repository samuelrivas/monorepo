// Copyright (C) 2018 by samuelrivas@gmail.com

#include <iostream>

using std::cout;
using std::endl;

class Base {
 public:
  Base() {
    cout << "Constructing Base" << endl;
  }
  ~Base() {
    cout << "Destructing Base" << endl;
  }

  void stuff() {
    cout << "doing stuff in Base" << endl;
  }
};

class VirtualBase {
 public:
  VirtualBase() {
    cout << "constructing VirtualBase" << endl;
  }
  virtual ~VirtualBase() {
    cout << "destructing VirtualBase" << endl;
  }

  virtual void stuff() {
    cout << "doing stuff in VirtualBase" << endl;
  }
};

class Derived : public Base {
 public:
  Derived() {
    cout << "Constructing Derived" << endl;
  }
  ~Derived() {
    cout << "Destructing Derived" << endl;
  }

  void stuff() {
    cout << "doing stuff in Derived" << endl;
  }
};

class VirtualDerived : public VirtualBase {
 public:
  VirtualDerived() {
    cout << "constructing VirtualDerived" << endl;
  }
  virtual ~VirtualDerived() {
    cout << "destructing VirtualDerived" << endl;
  }

  virtual void stuff() {
    cout << "doing stuff in VirtualDerived" << endl;
  }
};

int main(void) {
  {
    cout << "Constructing structs" << endl;

    Base base;
    cout << endl;
    Derived derived;
    cout << endl;
    VirtualBase virtualBase;
    cout << endl;
    VirtualDerived virtualDerived;

    cout << "Using methods" << endl;

    base.stuff();
    virtualBase.stuff();
    derived.stuff();
    virtualDerived.stuff();

    cout << "Going out of scope" << endl;
  }

  {
    cout << "Allocating pointers" << endl;

    Base* base = new Base;
    cout << endl;
    Derived* derived = new Derived;
    cout << endl;
    VirtualBase* virtualBase = new VirtualBase;
    cout << endl;
    VirtualDerived* virtualDerived = new VirtualDerived;
    cout << endl;

    cout << "Using methods" << endl;

    base -> stuff();
    virtualBase -> stuff();
    derived -> stuff();
    virtualDerived -> stuff();

    cout << "Freeing pointers" << endl;

    delete base;
    cout << endl;
    delete virtualBase;
    cout << endl;
    delete derived;
    cout << endl;
    delete virtualDerived;
    cout << endl;
  }

  {
    cout << "Allocating pointers to base" << endl;

    Base* base = new Base;
    cout << endl;
    Base* derived = new Derived;
    cout << endl;
    VirtualBase* virtualBase = new VirtualBase;
    cout << endl;
    VirtualBase* virtualDerived = new VirtualDerived;
    cout << endl;

    cout << "Using methods" << endl;

    base -> stuff();
    virtualBase -> stuff();
    derived -> stuff();
    virtualDerived -> stuff();

    cout << "Freeing pointers" << endl;

    delete base;
    cout << endl;
    delete virtualBase;
    cout << endl;
    delete derived;
    cout << endl;
    delete virtualDerived;
    cout << endl;
  }

  return 0;
}
