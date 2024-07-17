// main.cpp -- Driver program for the Video Store Inventory System
// CSC 122, Fall 2002
// Based on Carrano & Prichard, pp. 212-218
// Programmer: Brian Howard
// Created: November 10, 2002
// Last Modified: November 13, 2002

#include <iostream>
#include <string>
#include <fstream>
using namespace std;
#include <ctype.h>
#include <stdlib.h>
#include "getline.h"
#include "inventory.h"

void doHelp()
// Display the list of available commands
{
    cout << "H        (help)     Provide a summary of commands" << endl;
    cout << "I<title> (inquire)  Display inventory information for the title" << endl;
    cout << "L        (list)     List the entire inventory (alpha by title)" << endl;
    cout << "A<title> (add)      Add a new title" << endl;
    cout << "M<title> (modify)   Change the number of copies wanted" << endl;
    cout << "D        (delivery) Take delivery from shipment.txt" << endl;
    cout << "O        (order)    Write a purchase order" << endl;
    cout << "R        (return)   Write and execute a return order" << endl;
    cout << "S<title> (sell)     Sell or reserve a copy of the title" << endl;
    cout << "Q        (quit)     Save inventory.txt and exit" << endl;
} // end doHelp

void doInquire(const Inventory &inventory, string title)
// Search for a StockItem in the inventory that matches the title;
// Display its information on cout if found, or print a message if not
{
    StockItem item = inventory.find(title);

    if (item.isNull()) {
        cout << "Title not found" << endl;
        return;
    }

    item.display();
} // end doInquire

void listVisitor(const StockItem &item)
// Display one stock item
{
    item.display();
} // end listVisitor

void doList(const Inventory &inventory)
// Display all the StockItems in the inventory in alphabetical order
{
    inventory.map(listVisitor);
} // end doList

void doAdd(Inventory &inventory, string title, int want)
// Insert a new StockItem with a given want value into the inventory
// Complains if a StockItem with the given title already exists
{
    StockItem item = inventory.find(title);

    if (item.isNull()) {
        inventory.insert(StockItem(title, want, 0));
    } else {
        cout << "Title already exists" << endl;
    }
} // end doAdd

void doModify(Inventory &inventory, string title, int want)
// Change the want level for a given title in the inventory
// Complains if the title does not already exist
{
    StockItem item = inventory.find(title);

    if (item.isNull()) {
        cout << "Title not found" << endl;
        return;
    }

    item.want = want;
    inventory.replace(item);
} // end doModify

void doDelivery(Inventory &inventory)
// Process the file shipment.txt containing alternating lines giving
// titles and the number of copies deliverd; add the copies to the
// inventory and set aside any copies that were reserved on a wait list
{
    ifstream inFile("shipment.txt");
    string title, countString;
    int count;

    while (getline(inFile, title)) {
        getline(inFile, countString);
        count = atoi(countString.c_str());

        StockItem item = inventory.find(title);

        if (item.isNull()) {
            // title was not previously in inventory, so add it (with want = 0)
            inventory.insert(StockItem(title, 0, count));
        } else {
            // Add the new copies to the stock
            item.have += count;

            // If there were names on the wait list, set copies aside
            while (count > 0 && item.hasWait()) {
                count--;
                string name = item.removeWait();
                cout << "Set aside one copy of " << item.title
                    << " for " << name << endl;
            }

            // Update the inventory record for this title
            inventory.replace(item);
        }
    }
} // end doDelivery

void purchaseVisitor(const StockItem &item)
// Produce purchase order for one StockItem
{
    if (item.have < item.want) {
        cout << item.title << ": " << (item.want - item.have) << endl;
    }
} // end purchaseVisitor

void doOrder(const Inventory &inventory)
// Write a purchase order to cout for enough videos to bring
// each StockItem up to its want level
{
    cout << "Purchase Order:" << endl;
    cout << "---------------" << endl;
    inventory.map(purchaseVisitor);
    cout << endl;
} // end doOrder

void returnVisitor(StockItem &item)
// Prepare a return order for one StockItem;
// Reduces the have level if greater than the want level
{
    if (item.have > item.want) {
        cout << item.title << ": " << (item.have - item.want) << endl;
        item.have = item.want;
    }
} // end returnVisitor

void doReturn(Inventory &inventory)
// Write a return order to cout for all the excess videos (where the
// have value is greater than the want value)
{
    cout << "Return Order:" << endl;
    cout << "-------------" << endl;
    inventory.map(returnVisitor);
    cout << endl;
} // end doReturn

void doSell(Inventory &inventory, string title)
// Sell one copy of the given title; if not in stock, add the customer
// name to the title's wait list
// Complain if the title does not exist
{
    StockItem item = inventory.find(title);
    
    if (item.isNull()) {
        cout << "Title not found" << endl;
        return;
    }

    item.have--;
    if (item.have < 0) {
        string name;

        cout << "Out of stock; please enter name for wait list: ";
        getline(cin, name);
        item.addWait(name);
    }

    // Update the inventory to reflect the sale
    inventory.replace(item);
} // end doSell

void doQuit()
// Print a farewell message
{
    cout << "Good-bye!" << endl;
} // end doQuit

void doUnknown()
// Print a helpful message when the user doesn't know what they're doing
{
    cout << "Unknown command; use H for help" << endl;
} // end doUnknown

void getTitle(string &title)
// Read a line of input into title; repeat until not ""
{
    getline(cin, title);
    while (title == "") {
        cout << "You must enter a non-empty title: ";
        getline(cin, title);
    }
} // end getTitle

int main ()
// The main driver for the video store inventory program
// Repeatedly accept a command and execute it until the user quits
{
    // Restore the inventory from the file inventory.txt
    Inventory inventory("inventory.txt");

    char command;
    string title;
    int want;
    
    do {
        cout << "? ";
        cin >> command;
        command = toupper(command);

        switch (command) {
            case 'H':
                doHelp();
                break;

            case 'I':
                getTitle(title);
                doInquire(inventory, title);
                break;

            case 'L':
                doList(inventory);
                break;

            case 'A':
                getTitle(title);
                cout << "Initial want value? ";
                cin >> want;
                doAdd(inventory, title, want);
                break;

            case 'M':
                getTitle(title);
                cout << "New want value? ";
                cin >> want;
                doModify(inventory, title, want);
                break;

            case 'D':
                doDelivery(inventory);
                break;

            case 'O':
                doOrder(inventory);
                break;

            case 'R':
                doReturn(inventory);
                break;

            case 'S':
                getTitle(title);
                doSell(inventory, title);
                break;

            case 'Q':
                doQuit();
                break;

            default:
                doUnknown();
                break;
        }                
    } while (command != 'Q');
    
    return 0;
    // When inventory gets destroyed, it will be preserved in inventory.txt
    // through the magic of destructor functions
    
} // end main
