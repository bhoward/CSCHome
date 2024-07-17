// Model Solution for Programming Project 1
// CSC 122, Computer Science II, Fall 2002
// Brian Howard, September 10, 2002

// Carrano & Prichard, Programming Problem 1.2 (page 50)
// "Write a program that sorts and evaluates bridge hands."

#include <iostream>
#include <ctype.h>    // should be <cctype>
#include <string>
using namespace std;

// Define the relevant sizes for a deck of cards
const int NUM_SUITS = 4;
const int NUM_RANKS = 13;
const int HAND_SIZE = 13;

// This is the type that will be used to store a hand of cards
// If hand is of HandType, then hand[suit][rank] is true when the
//   hand of cards contains the card with the given suit and rank.
typedef bool HandType[NUM_SUITS][NUM_RANKS];

// Clear all the cards from a hand
// Precondition: none
// Postcondition: all the entries in hand have been cleared
void clearHand(HandType hand)
{
    for (int suit = 0; suit < NUM_SUITS; suit++) {
        for (int rank = 0; rank < NUM_RANKS; rank++) {
            hand[suit][rank] = false;
        } // end for rank
    } // end for suit
} // end clearHand

// Convert a character representing a rank into an integer index
// Precondition: rankChar is one of '2', ..., '9', 'T', 'J', 'Q', 'K', 'A'
//   (lower-case versions also accepted)
// Postcondition: returns an integer index from 0 to 12 giving the index
//   to use with HandType for the corresponding rank of card
int rankIndex(char rankChar)
{
    switch (toupper(rankChar)) {
    case '2': return 0;
    case '3': return 1;
    case '4': return 2;
    case '5': return 3;
    case '6': return 4;
    case '7': return 5;
    case '8': return 6;
    case '9': return 7;
    case 'T': return 8;
    case 'J': return 9;
    case 'Q': return 10;
    case 'K': return 11;
    case 'A': return 12;
    } // end switch rankChar
} // end rankIndex

// Convert a character representing a suit into an integer index
// Precondition: suitChar is one of 'C', 'D', 'H', 'S'
//   (lower-case versions also accepted)
// Postcondition: returns an integer index from 0 to 3 giving the index
//   to use with HandType for the corresponding suit of card
int suitIndex(char suitChar)
{
    switch (toupper(suitChar)) {
    case 'C': return 0;
    case 'D': return 1;
    case 'H': return 2;
    case 'S': return 3;
    } // end switch suitChar
} // end suitIndex

// Convert a numeric rank index into a string representation
// Precondition: rank is in the range 0 to 12
// Postcondition: returns a two-character string naming the rank, using
//   the same encoding as rankIndex
string rankName(int rank)
{
    switch (rank) {
    case 0:  return " 2";
    case 1:  return " 3";
    case 2:  return " 4";
    case 3:  return " 5";
    case 4:  return " 6";
    case 5:  return " 7";
    case 6:  return " 8";
    case 7:  return " 9";
    case 8:  return "10";
    case 9:  return " J";
    case 10: return " Q";
    case 11: return " K";
    case 12: return " A";
    } // end switch rank
} // end rankName

// Convert a numeric suit index into a string representation
// Precondition: suit is in the range 0 to 3
// Postcondition: returns an eight-character string naming the suit, using
//   the same encoding as suitIndex
string suitName(int suit)
{
    switch (suit) {
    case 0:  return "CLUBS   ";
    case 1:  return "DIAMONDS";
    case 2:  return "HEARTS  ";
    case 3:  return "SPADES  ";
    } // end switch suit
} // end suitName

// Read in one character pair representing a card and record it in the hand
// Precondition: a pair of characters representing a rank and a suit are
//   ready to be read in from cin
// Postcondition: the corresponding entry in hand is set to true;
//   no error checking is done in this version
void readCard(HandType hand)
{
    char rankChar, suitChar;

    cin >> rankChar >> suitChar;

    int rank = rankIndex(rankChar);
    int suit = suitIndex(suitChar);

    hand[suit][rank] = true;
} // end readCard

// Read in a hand of cards from cin
// Precondition: there should be HAND_SIZE pairs of characters ready on cin
// Postcondition: the cards will be recorded in hand
void readHand(HandType hand)
{
    clearHand(hand);
    for (int i = 0; i < HAND_SIZE; i++) {
        readCard(hand);
    } // end for i
} // end readhand

// Compute the score of a bridge hand, as described in the text
// Precondition: hand contains information about one hand of cards
// Postcondition: returns the score of the hand
int scoreHand(HandType hand)
{
    int score = 0;

    // Process each suit in turn
    for (int suit = 0; suit < NUM_SUITS; suit++) {
        // Check for face cards:
        if (hand[suit][rankIndex('A')]) score += 4;
        if (hand[suit][rankIndex('K')]) score += 3;
        if (hand[suit][rankIndex('Q')]) score += 2;
        if (hand[suit][rankIndex('J')]) score += 1;

        // Count the number of cards in the suit
        int count = 0;
        for (int rank = 0; rank < NUM_RANKS; rank++) {
            if (hand[suit][rank]) count++;
        } // end for rank

        if (count == 0) score += 3; // Void
        if (count == 1) score += 2; // Singleton
        if (count == 2) score += 1; // Doubleton
        if (count > 5) score += count - 5; // Long suit
    } // end for suit

    return score;
} // end scoreHand

// Display the contents of a hand of cards on cout
// Precondition: hand contains information about one hand of cards
// Postcondition: the hand has been displayed, one suit per line
void displayHand(HandType hand)
{
    for (int suit = 0; suit < NUM_SUITS; suit++) {
        cout << suitName(suit);
        // Check the cards backwards, to print from highest to lowest rank
        for (int rank = NUM_RANKS - 1; rank >= 0; rank--) {
            if (hand[suit][rank]) {
                cout << "   " << rankName(rank);
            } // end if
        } // end for rank
        cout << endl;
    } // end for suit
} // end displayHand

// The main program: read hands of cards and display them with their scores
// until the end-of-file (Ctrl-Z on Windows) is reached
int main()
{
    HandType hand;

    // loop until end-of-file
    while (cin >> ws && cin.peek() != EOF) {
        readHand(hand);
        displayHand(hand);
        cout << "Points = " << scoreHand(hand) << endl << endl;
    } // end while

    return 0;
} // end main
