/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   main.cpp
 * Author: Zuzka
 *
 * Created on 09 March 2020, 11:48
 */

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <fstream>
#include <string>
#include <vector>
#include <climits>

using namespace std;

  class Vertex {
  public:
      
      vector<int> sousedi;
      vector<double> vahy;
      
      Vertex (){
      }
      
      
  };

class Graph { 
  private:
    
      vector<Vertex> uzly;
      double best_sol_number; 
      vector<int> best_sol_vector;
      int citac;
      int n;
      int a;
      
  public:
    
    Graph ( int nn, int aa ){
        best_sol_number=INT_MAX;
        uzly.resize(nn);
        best_sol_vector.resize(nn);
        citac=0;
        n=nn;
        a=aa;
    }
    
     ~Graph  ( void ){ }
     
    void addEdge(int u1, int u2, double v){
        uzly[u1].sousedi.push_back(u2);
        uzly[u1].vahy.push_back(v);
        uzly[u2].sousedi.push_back(u1);
        uzly[u2].vahy.push_back(v);   
    } 
    
    void printGraph(void){
        
        for (int i = 0; i < uzly.size(); i++) { 
        cout << "vrchol: " << i << "\n";
        cout<< " sousedi: ";
        for (auto x : uzly[i].sousedi) 
           cout << " " << x; 
        printf("\n"); 
        }
        
        for (int i = 0; i < uzly.size(); i++) { 
        cout << "vrchol: " << i << "\n";
        cout<< " vahy: ";
        for (auto x : uzly[i].vahy) 
           cout << " " << x; 
        printf("\n"); 
        } 
        
    }
    
    void printResult(void){
        
        cout << "Vysledek: " << best_sol_number << " \n";
    
        for(auto f=best_sol_vector.begin(); f!= best_sol_vector.end(); ++f){
            cout << (*f);
        }
        cout << "\n"; 
        cout<< "pocet volani funkce: "<< citac << " \n";
    }
    
    //hranovy rez pri uplnem ohodnoceni vrcholu
    double hranovyRezALL (const vector<int> & vysl, int i, double rez){
        
        i--;

        for (int j=i; j<n;j++){
            for(int x=0; x<uzly[j].sousedi.size();x++){
                    if(vysl[j]!=vysl[uzly[j].sousedi[x]]){
                        rez+=uzly[j].vahy[x];
                    }

              }
        }
 
        return rez;
        
    }
    
    //dilci hranovy rez pri neuplnem ohodnoceni vrcholu
    double hranovyRezN (const vector<int> & vysl, int i, double rez){
 
        if (i==0) return 0;
        i--;
        
        for(int x=0; x<uzly[i].sousedi.size();x++){
            
            //uvazuju pouze dosud ohodnocene vrcholy
            if (uzly[i].sousedi[x]<=i){
                //pokud jsou v jine mnozine, pridam jejich vahu do rezu
                if(vysl[i]!=vysl[uzly[i].sousedi[x]]){
                    rez+=uzly[i].vahy[x];
                }
                
            }
            
        }
 
        return rez;
    }

    //rekurzivní funkce pro výpočet minimálního hranového řezu  
    void mhrN( int akt_i, int akt_uz, vector<int> & vysl, double rez){  

        citac++;
       
        if (n-akt_uz+akt_i < a){ 
            //cout<< "rezu kvuli a"<< "\n";
            return;
        }

        //mnozina a je naplnena
        if (akt_i == a){  

            rez=hranovyRezALL(vysl,akt_uz,rez);

            if (rez < best_sol_number){
                best_sol_number=rez;
                best_sol_vector=vysl;
            }
            return;  
        }  
        //v mnozine a jeste zbyva misto
        else {

        rez=hranovyRezN(vysl,akt_uz, rez);

        if (rez > best_sol_number){
            return;
            }

        }

        if (akt_uz >= n)  {
            return; 
        }
        
            //pridavam uzel na aktualnim indexu
            vysl[akt_uz]=1;
            mhrN( akt_i + 1, akt_uz + 1, vysl, rez);  
            
            // nepridavam uzel na aktualnim indexu
            vysl[akt_uz]=0;
            mhrN( akt_i, akt_uz + 1, vysl, rez); 
            

    }  
    
};
 
int main(int argc, char** argv) {
    int n, k, a, u1, u2;
    double v;
    string s;
    
    if (argv[1]==NULL){
        s="mhr_data/mhr_30_10_10.txt";
    }
    else s=argv[1];
    
    ifstream infile (s);

    if (!infile.is_open()){
        cout << "Soubor  nelze nacist";
        return 0;
    }
    
    if (!(infile >> n >> k >> a)) {return 1;}
    cout << "n: " << n << " k: " << k << " a: " << a << "\n";
    
    Graph g(n, a);
    
    while (infile >> u1 >> u2 >> v){
        g.addEdge(u1,u2,v);
    }
    
    infile.close();

    vector<int> vysl(n);
    g.mhrN(0,0,vysl,0);
    g.printResult();

    return 0;
}

