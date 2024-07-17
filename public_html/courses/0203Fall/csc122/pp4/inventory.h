// inventory.h -- Interface for the Inventory ADT
// CSC 122, Fall 2002
// Based on Carrano & Prichard, pp. 212-218
// Programmer: Brian Howard
// Created: November 10, 2002
// Last Modified: November 12, 2002

#ifndef INVENTORY_H
#define INVENTORY_H

#include "stockitem.h"

class Inventory {
public:
    // Construct a new Inventory object by reading the file fn
    Inventory(const string &fn);

    // Destroy the Inventory object, first saving it back in filename
    ~Inventory();

    // Insert a new StockItem in the inventory, keeping the list
    // alphabetized by title
    void insert(const StockItem &item);
    
    // Overwrite an existing StockItem having the given item's title
    // (updates the have and want fields, plus the waiting list)
    void replace(const StockItem &item);
    
    // Search for a StockItem with the given title; if not found,
    // returns a null StockItem (the title is "")
    StockItem find(const string &title) const;

    // This is the type of a visitor function for the map operation;
    // it takes a reference to a StockItem and returns nothing
    typedef void Visitor(StockItem &item);

    // Map a visitor function by applying it to all the StockItems in the
    // inventory (in alphabetical order)
    void map(Visitor visitor);
    
    // Here are the const versions of the visitor type and map function,
    // which promise not to modify the inventory:
    typedef void ConstVisitor(const StockItem &item);
    void map(ConstVisitor visitor) const;    
private:
    // The file from which the inventory was read when it was constructed 
    const string filename;

    // The struct that represents one node in the linked list of StockItems
    struct StockNode {
        StockItem item;
        StockNode *next;

        StockNode(const StockItem &it) : item(it), next(0) {}
    };

    // The head node of the linked list
    StockNode *head;
    
    // By declaring the copy constructor in the private section (and never
    // implementing it), we guarantee that Inventory objects will not be copied
    Inventory(const Inventory &);

    // Same for the assignment operator, to prevent copying by assignment
    Inventory &operator=(const Inventory &);
};

#endif