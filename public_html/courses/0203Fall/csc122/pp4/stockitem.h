// stockitem.h -- Interface for the StockItem ADT
// CSC 122, Fall 2002
// Based on Carrano & Prichard, pp. 212-218
// Programmer: Brian Howard
// Created: November 10, 2002
// Last Modified: November 12, 2002

#ifndef STOCKITEM_H
#define STOCKITEM_H

class StockItem {
public:
    // The title of the item
    const string title;

    // The desired (target) stock value (the level we "want" to have)
    // and the current stock value (the number we "have" in stock)
    // If have is negative, it reflects the number of people on the wait list
    int want, have;

    // Construct a new StockItem, with default title "" and zero want/have
    StockItem(string t = "", int w = 0, int h = 0);

    // Returns true if the StockItem is the "null" item -- the title is ""
    bool isNull() const;

    // Print a description of the StockItem on cout
    void display() const;

    // Add a name to the rear of the waiting list
    // (does not change the have value)
    void addWait(const string &name);

    // Returns true if there are names on the waiting list
    bool hasWait() const;

    // Removes the name at the front of the waiting list and returns it
    string removeWait();

    // Copies the want, have, and waiting list pointers from item
    // (does not change the title)
    StockItem &operator=(const StockItem &item);

private:
    // A node on the waiting list, containing one name
    struct WaitNode {
        string who;
        WaitNode *next;
    };

    // waitHead points to the front of the waiting list, waitTail to the rear
    // If both are zero, there is no one waiting
    WaitNode *waitHead, *waitTail;
};

#endif
