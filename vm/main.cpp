#include "vm.hpp"

using namespace std;

int main(int argc, char** argv) {
    if (argc != 2) {
        cerr << "filename expected" << endl;
        return 0;
    }
    vm vm{1000000, 1000000, argv[1]};
    vm.exec();
    cout << vm << endl;
}