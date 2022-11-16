#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <ctime>

std::fstream out("out.txt");

double info(std::vector<std::vector<char>> data) {
    double buff = 0;
    int count = 0;
    for (std::vector<char> i : data) {
        if (i[0] == 'e') {
            count++;
        }
    }
    int ob = data.size() - count;
    if (count != 0)buff = -((double) count / (double) data.size()) * log2((double) count / (double) data.size());
    if (ob != 0)buff -= ((double) ob / (double) data.size()) * log2((double) ob / (double) data.size());
    return buff;
}

double infoX(int n, std::vector<std::vector<char>> data) {
    std::vector<char> type;
    for (int i = 0; i < data.size(); i++) {
        if (find(type.begin(), type.end(), data[i][n]) == type.end()) {
            type.push_back(data[i][n]);
        }
    }
    std::vector<int> countT(type.size());
    std::vector<std::vector<std::vector<char>>> T(type.size());
    for (int i = 0; i < data.size(); i++) {
        for (int t = 0; t < type.size(); t++) {
            if (data[i][n] == type[t]) {
                countT[t]++;
                T[t].push_back(data[i]);
                break;
            }
        }
    }
    double buff = 0;
    for (int i = 0; i < type.size(); i++) {
        buff += ((double) countT[i] / (double) data.size()) * info(T[i]);
    }
    return buff;
}

double split(int n, std::vector<std::vector<char>> data) {
    std::vector<char> type;
    for (int i = 0; i < data.size(); i++) {
        if (find(type.begin(), type.end(), data[i][n]) == type.end()) {
            type.push_back(data[i][n]);
        }
    }
    std::vector<int> countT(type.size());
    std::vector<std::vector<std::vector<char>>> T(type.size());
    for (int i = 0; i < data.size(); i++) {
        for (int t = 0; t < type.size(); t++) {
            if (data[i][n] == type[t]) {
                countT[t]++;
                T[t].push_back(data[i]);
                break;
            }
        }
    }
    double buff = 0;
    for (int i = 0; i < type.size(); i++) {
        buff -= ((double) countT[i] / (double) data.size()) * log2((double) countT[i] / (double) data.size());
    }
    return buff;
}

double gain(int n, std::vector<std::vector<char>> data) {
    return (info(data) - infoX(n, data)) / split(n, data);
}

class node {
private:
    bool sed;
    double ver;
    int p = -1;
    std::vector<node> childs;
    std::vector<int> np;
    std::vector<std::vector<char>> data;
public:
    node(std::vector<int> np, std::vector<std::vector<char>> data) {
        this->np = np;
        this->data = data;
    }

    void create_childs() {
        if (np.empty())return;
        double gainp = 0;
        for (int i = 0; i < np.size(); i++) {
            if (gainp <= gain(np[i], data)) {
                gainp = gain(np[i], data);
                p = np[i];
            }
        }
        if (gainp == 0) return;
        std::vector<char> type;
        for (int i = 0; i < data.size(); i++) {
            if (find(type.begin(), type.end(), data[i][p]) == type.end()) {
                type.push_back(data[i][p]);
            }
        }
        std::vector<std::vector<std::vector<char>>> dates(type.size());
        for (int i = 0; i < data.size(); i++) {
            for (int t = 0; t < type.size(); t++) {
                if (data[i][p] == type[t]) {
                    dates[t].push_back(data[i]);
                }
            }
        }
        std::vector<std::vector<int>> nnp(type.size());
        for (int i = 0; i < np.size(); i++) {
            if (np[i] != p) {
                for (int t = 0; t < type.size(); t++)nnp[t].push_back(np[i]);
            }
        }
        for (int i = 0; i < type.size(); i++) {
            node child(nnp[i], dates[i]);
            childs.push_back(child);
        }
    }

    void isSed() {
        int count = 0;
        for (int i = 0; i < data.size(); i++) {
            if (data[i][0] == 'e')count++;
        }
        sed = 2 * count > data.size() ? true : false;
        ver = count / (double) data.size();
    }

    void tree_create() {
        create_childs();
        if (childs.empty()) {
            isSed();
        } else
            for (int i = 0; i < childs.size(); i++) {
                childs[i].tree_create();
            }

    }

    std::pair<int, int> accuracy() {
        int count = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'e') {
                    count++;
                } else if (!sed && data[i][0] == 'p') {
                    count++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count += childs[i].accuracy().first;
            }
        }
        return {count, data.size()};
    }

    double acc() {
        std::pair<int, int> t = accuracy();
        return t.first / (double) t.second;
    }

    std::pair<int, int> precision() {
        int count1 = 0, count2 = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'e') {
                    count1++;
                }
                if (sed && data[i][0] == 'p') {
                    count2++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count1 += childs[i].precision().first;
                count2 += childs[i].precision().second;
            }
        }
        return {count1, count2};
    }

    double pr() {
        std::pair<int, int> t = precision();
        return t.first / (double) (t.second + t.first);
    }

    std::pair<int, int> recall() {
        int count1 = 0, count2 = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'e') {
                    count1++;
                }
                if (!sed && data[i][0] == 'e') {
                    count2++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count1 += childs[i].recall().first;
                count2 += childs[i].recall().second;
            }
        }
        return {count1, count2};
    }

    double re() {
        std::pair<int, int> t = recall();
        return t.first / (double) (t.second + t.first);
    }

    std::pair<int, int> fpr_alg() {
        int count1 = 0, count2 = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'p') {
                    count1++;
                }
                if (!sed && data[i][0] == 'e') {
                    count2++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count1 += childs[i].fpr_alg().first;
                count2 += childs[i].fpr_alg().second;
            }
        }
        return {count1, count2};
    }

    double fpr() {
        std::pair<int, int> t = fpr_alg();
        return t.first / (double) (t.second + t.first);
    }

    void print() {
        for (int i = 0; i < childs.size(); i++) {
            std::cout << p << " " << info(childs[i].data) << std::endl;
            childs[i].print();
        }
    }

    void prver() {
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++)out << ver << ", ";
        } else
            for (int i = 0; i < childs.size(); i++) {
                childs[i].prver();
            }
    }

    void prsed() {
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
               if(sed) out << "1, ";
               else out << "0, ";
            }
        } else
            for (int i = 0; i < childs.size(); i++) {
                childs[i].prsed();
            }
    }

    void prreal() {
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if(data[i][0]=='e') out << "1, ";
                else out << "0, ";
            }
        } else
            for (int i = 0; i < childs.size(); i++) {
                childs[i].prreal();
            }
    }

    int tp() {
        int count = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'e') {
                    count++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count += childs[i].tp();
            }
        }
        return count;
    }

    int fp() {
        int count = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (sed && data[i][0] == 'p') {
                    count++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count += childs[i].fp();
            }
        }
        return count;
    }

    int tn() {
        int count = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (!sed && data[i][0] == 'p') {
                    count++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count += childs[i].tn();
            }
        }
        return count;
    }

    int fn() {
        int count = 0;
        if (childs.empty()) {
            for (int i = 0; i < data.size(); i++) {
                if (!sed && data[i][0] == 'e') {
                    count++;
                }
            }
        } else {
            for (int i = 0; i < childs.size(); i++) {
                count += childs[i].fn();
            }
        }
        return count;
    }
};


int main() {
    std::fstream in("agaricus-lepiota.data");
    std::string a;
    std::vector<std::vector<char>> data;
    while (in >> a) {
        std::vector<char> buff;
        while (!a.empty()) {
            buff.push_back(a.front());
            a.erase(0, 2);
        }
        data.push_back(buff);
    }
    int n = sqrt(data[0].size() - 1);
    std::vector<int> np;
    srand(time(nullptr));
    for (int i = 0; i < n; i++) {
        int p = (rand() / (double) RAND_MAX) * data[0].size();
        if (p != 0 && find(np.begin(), np.end(), p) == np.end()) {
            np.push_back(p);
        } else i--;
    }

    node mn(np, data);

    mn.tree_create();

    std::cout << "accurancy " << mn.acc() << std::endl;

    std::cout << "percision " << mn.pr() << std::endl;

    std::cout << "recall " << mn.re() << std::endl; // tpr

    std::cout << "fpr " << mn.fpr() << std::endl;

    std::cout << "tp " << mn.tp() << " tn " << mn.tn() << " fp " << mn.fp() << " fn " << mn.fn() << std::endl;

    std::cout << std::endl;
    mn.print();
    mn.prver();
    out<<std::endl;
    mn.prsed();
    out<<std::endl;
    mn.prreal();
    return 0;
}