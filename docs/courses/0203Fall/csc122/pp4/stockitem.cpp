// stockitem.cpp -- Implementation for the StockItem ADT
// CSC 122, Fall 2002
// Based on Carrano & Prichard, pp. 212-218
// Programmer: Brian Howard
// Created: November 12, 2002
// Last Modified: November 13, 2002
//
// Contains 2 ** TODO ** items

#include <string>
using namespace std;
#include "stockitem.h"

StockItem::StockItem(string t, int w, int h)
    : title(t), want(w), have(h), waitHead(0), waitTail(0) {}

bool StockItem::isNull() const
{
    return title == "";
}

void StockItem::display() const
{
    cout << '"' << title << '"'
         << ": want " << want;

    // Now display the wait list, if any
    if (hasWait()) {
        cout << "; none in stock" << endl;
        cout << "People waiting are:" << endl;
        WaitNode *cur = waitHead;
        while (cur != 0) {
            cout << "  " << cur->who << endl;
            cur = cur->next;
        }
    } else {
        cout << "; have " << have << endl;
    }
}

void StockItem::addWait(const string &name)
{

    // ** TODO **
    // Append a new WaitNode containing the string name
    // to the rear of the wait list
    // Be sure to handle an initially empty list correctly

}

bool StockItem::hasWait() const
{
    return waitHead != 0;
}

string StockItem::removeWait()
{

    // ** TODO **
    // Remove an item from the front of the wait list;
    // Delete the node and return the string it contained

}

StockItem &StockItem::operator=(const StockItem &item)
{
    // Copy all the fields except the title (which can't change)
    want = item.want;
    have = item.have;
    waitHead = item.waitHead;
    waitTail = item.waitTail;
    return *this;
}
    
