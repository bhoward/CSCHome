// inventory.cpp -- Implementation for the Inventory ADT
// CSC 122, Fall 2002
// Based on Carrano & Prichard, pp. 212-218
// Programmer: Brian Howard
// Created: November 12, 2002
// Last Modified: November 13, 2002
//
// Contains 3 ** TODO ** items

#include <string>
#include <fstream>
using namespace std;
#include "getline.h"
#include "inventory.h"

Inventory::Inventory(const string &fn) : filename(fn), head(0) {
    ifstream inFile(filename.c_str());
    string title;
    StockNode *prev = 0;

    while (getline(inFile, title)) {
        int want, have;

        inFile >> want >> have;
        inFile.get(); // read the newline after the have value
        
        StockItem item(title, want, have);

        // Read in the wait list, if any
        while (have < 0) {
            string name;
            
            have++;
            getline(inFile, name);
            item.addWait(name);
        }

        // Append a new node at the end of the inventory
        StockNode *temp = new StockNode(item);
        if (head == 0) {
            head = temp;
        } else {
            prev->next = temp;
        }
        prev = temp;
    }
}

Inventory::~Inventory() {
    ofstream outFile(filename.c_str());

    while (head != 0) {
        StockItem item = head->item;

        // Only write items with non-zero want or have values
        if (item.want != 0 || item.have != 0) {
            outFile << item.title << endl;
            outFile << item.want << " " << item.have << endl;

            // Write the wait list as it gets cleared:
            while (item.hasWait()) {
                outFile << item.removeWait() << endl;
            }
        }

        StockNode *temp = head;
        head = head->next;
        temp->next = 0;
        delete temp;
        temp = 0;
    }
}

void Inventory::insert(const StockItem &item) {
    StockNode *temp = new StockNode(item);

    // ** TODO **
    // Insert the new node, pointed to by temp, into the inventory
    // in the correct position (alphabetically by title)
    //
    // Make sure you handle insertion at the head correctly

}
void Inventory::replace(const StockItem &item) {

    // ** TODO **
    // Find the StockNode on the inventory list whose title matches
    // the given item's title, and replace the other information about
    // that StockItem with the information from item (if "cur" points to
    // the found node, then "cur->item = item;" will do the replacement)

}

StockItem Inventory::find(const string &title) const
{

    // ** TODO **
    // Find a StockItem in the inventory with the given title and return it
    // If the item is not found, return a "null" item: "return StockItem();"

}

void Inventory::map(Visitor visitor) {
    for (StockNode *cur = head; cur != 0; cur = cur->next) {
        visitor(cur->item);
    }
}

void Inventory::map(ConstVisitor visitor) const {
    for (StockNode *cur = head; cur != 0; cur = cur->next) {
        visitor(cur->item);
    }
}
