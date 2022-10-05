#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <array>

using namespace std;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/


namespace Const
{
    constexpr int BoardWidth {6};
    constexpr int BoardHeight {12};
}

using TBoardItem = char;
using TBoardType = std::array<TBoardItem, Const::BoardWidth * Const::BoardHeight>;

namespace helpers_func
{
    // 0 height == down
    int get_id_by_coord(const int height, const int width)
    {
        return (Const::BoardHeight - height) * Const::BoardWidth + width;
    }

    void print_debug_inf(const TBoardType& board)
    {
        cerr << "--Print Board--\n";
        //TODO
        for (int height_id = Const::BoardHeight - 1 ; height_id >= 0; --height_id)
        {
            for (int width_id = 0; width_id < Const::BoardWidth; ++width_id)
            {
                cerr << board[get_id_by_coord(height_id, width_id)];
            }
            cerr << "\n";
        }
        cerr << "---------------\n";
    }
    // todo: falldown
}

class Board
{
public:
    Board();

    const auto &get_board_data() { return m_board_data; };
private:
    TBoardType m_board_data;
};

int main()
{

    // game loop
    while (1) {
        for (int i = 0; i < 8; i++) {
            int color_a; // color of the first block
            int color_b; // color of the attached block
            cin >> color_a >> color_b; cin.ignore();
        }
        int score_1;
        cin >> score_1; cin.ignore();
        Board my_board {};
        helpers_func::print_debug_inf(my_board.get_board_data());
        int score_2;
        cin >> score_2; cin.ignore();
        for (int i = 0; i < 12; i++) {
            string row;
            cin >> row; cin.ignore();
        }

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;


        // "x rotation": the column in which to drop your pair of blocks followed by its rotation (0, 1, 2 or 3)
        cout << "0 1" << endl;
    }
}

Board::Board() {
    for (auto& item : m_board_data)
    {
        cin >> item;
    }
    cin.ignore();
    /*for (int item_id = 0 ; item_id < Const::BoardHeight * Const::BoardWidth; ++item_id)
    {
        cin >> m_board_data[item_id];
    }*/
    //m_board_data
}

/* Old solution
 * struct row{
    int length=1000;
    union{
        char sim;
        int num;
    };
};
*//**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **//*
int main()
{

    // game loop
    while (1) {
        int colorA, colorB;
        cin>>colorA>>colorB;
        for (int i = 0; i < 7; i++) {
            // color of the first block
            cin >> colorB >> colorB; cin.ignore();
        }
        int score1;
        cin >> score1; cin.ignore();

        row r[6],r1[6];
        for (int i = 0; i < 12; ++i)
            for(int j = 0; j < 6; ++j)
            {
                char t;
                cin>>t;
                if(t>='1' && t<='5' && r[j].length>i)
                {
                    r[j].sim=t;
                    r[j].length=i;
                }
                if(t>='0' && t<='5' && r1[j].length>i)
                {
                    r1[j].sim=t;
                    r1[j].length=i;
                }
            }
        int score2;
        cin >> score2; cin.ignore();
        for (int i = 0; i < 12; i++) {
            string rowt;
            cin >> rowt; cin.ignore();
        }
        row memory; memory.length=-1;
        for(int i=0;i<6;++i)
            cerr<<r[i].length<<" ";
        cerr<<endl;
        bool test=false;
        for(int i=0;i<6;++i)
        {
            cerr<<r1[i].length<<" ";
            if(r1[i].length>memory.length)
            {
                memory.length=r1[i].length;
                memory.num=i;
            }
            if(colorA==r[i].sim-48 && r1[i].length>1) {cerr<<endl<<r1[i].length<<" "<<r[i].length<<" len"<<endl; cout << i<<" 1"<< endl; test=true;}
        }
        if(test) continue;
        // To debug: cerr << "Debug messages..." << endl;


        // "x rotation": the column in which to drop your pair of blocks followed by its rotation (0, 1, 2 or 3)
        cerr<<endl<<memory.num<<" vih"<<endl;
        cout << memory.num<<" 1" << endl;
    }
}*/
