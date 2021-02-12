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
#include <omp.h>
#include <chrono>
#include <queue>
#include <bitset>
#include <mpi.h>

using namespace std;

  class Vertex {
  public:
      
      vector<int> sousedi;
      vector<double> vahy;
      
      Vertex (){
      }
      
      
  };
 
 struct Stav {
      uint64_t akt_i;
      uint64_t akt_uz;
      uint64_t vysl;
      double rez;
      
      Stav(int ai, int au,uint64_t v, double r ){
          akt_i=ai;
          akt_uz=au;
          vysl=v;
          rez=r;
      }
      Stav(){
      }
      
    };

class Graph {
  private:
    
      vector<Vertex> uzly;

      vector<int> best_sol_vector;
      int citac;
      int n;
      int a;
      
  public:
    double best_sol_number;
    uint64_t best_vector;
    
    Graph ( int nn, int aa ){
        best_sol_number=INT_MAX;
        best_vector=0;
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
        
        for(int i=0; i<n; i++){
            best_sol_vector[i] = (bool)(best_vector & (1 << i));
        }
    
        for(auto f=best_sol_vector.begin(); f!= best_sol_vector.end(); ++f){
            cout << (*f);
        }
        cout << "\n";
        cout<< "pocet volani funkce: "<< citac << " \n";
    }
    
    void printRes(double nej_vysl, uint64_t nej_vector){
        
        cout << "Vysledek: " << nej_vysl << " \n";
        
        for(int i=0; i<n; i++){
            best_sol_vector[i] = (bool)(nej_vector & (1 << i));
        }
    
        for(auto f=best_sol_vector.begin(); f!= best_sol_vector.end(); ++f){
            cout << (*f);
        }
        cout << "\n";
    }
    
    //hranovy rez pri uplnem ohodnoceni vrcholu
    double hranovyRezALL (uint64_t vysl, int i, double rez){
        
        i--;
        
        for (int j=i; j<n;j++){
            for(int x=0; x<uzly[j].sousedi.size();x++){
                    if((bool)(vysl & (1 << j))!= (bool)(vysl & (1 << uzly[j].sousedi[x]))){
                        rez+=uzly[j].vahy[x];
                    }

              }
        }
 
        return rez;
        
    }
    
    //dilci hranovy rez pri neuplnem ohodnoceni vrcholu
    double hranovyRezN (uint64_t vysl, int i, double rez){
 
        if (i==0) return 0;
        i--;
        
        for(int x=0; x<uzly[i].sousedi.size();x++){
            
            //uvazuju pouze dosud ohodnocene vrcholy
            if (uzly[i].sousedi[x]<=i){
                //pokud jsou v jine mnozine, pridam jejich vahu do rezu
                if((bool)(vysl & (1 << i))!=(bool)(vysl & (1 << uzly[i].sousedi[x]))){
                    rez+=uzly[i].vahy[x];
                }
                
            }
            
        }
        return rez;
    }

    //rekurzivní funkce pro výpo?et minimálního hranového ?ezu  
    void mhrN( int akt_i, int akt_uz, uint64_t vysl, double rez){  

        if (n-akt_uz+akt_i < a){
            return;
        }

        //mnozina a je naplnena
        if (akt_i == a){  

            rez=hranovyRezALL(vysl,akt_uz,rez);
            if (rez < best_sol_number){
                //kriticka sekce
                #pragma omp critical
                {
                //opetovna kontrola
                if (rez < best_sol_number){
                    best_sol_number=rez;
                    best_vector=vysl;
                    /*for(int i=0; i<n; i++){
                            best_sol_vector[i] = (bool)(vysl & (1 << i));
                        }*/
                }
                }
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
            vysl=(1 << akt_uz) | vysl;
            #pragma  omp  task
            mhrN( akt_i + 1, akt_uz + 1, vysl, rez);  
            
            // nepridavam uzel na aktualnim indexu
            vysl=vysl & (~(1 << akt_uz));
            
            //#pragma  omp  task
            mhrN( akt_i, akt_uz + 1, vysl, rez);
            //#pragma  omp  taskwait

    }  
    
    //z aktualniho stavu vygeneruje dalsi dva stavy
    vector<Stav> mhr_cs( Stav s){  

        vector<Stav> ns;
       
        if (n-s.akt_uz+s.akt_i < a){
            return ns;
        }

        //mnozina a je naplnena
        if (s.akt_i == a){  

            s.rez=hranovyRezALL(s.vysl,s.akt_uz,s.rez);
            if (s.rez < best_sol_number){
                //kriticka sekce
                #pragma omp critical
                {
                //opetovna kontrola
                if (s.rez < best_sol_number){
                    best_sol_number=s.rez;
                    for(int i=0; i<n; i++){
                            best_sol_vector[i] = (bool)(s.vysl & (1 << i));
                        }
                }
                }
            }
            return ns;  
        }  
        //v mnozine a jeste zbyva misto
        else {

        s.rez=hranovyRezN(s.vysl,s.akt_uz, s.rez);
        
            if (s.rez > best_sol_number){
            return ns;
            }     
        }

        if (s.akt_uz >= n)  {
            return ns;
        }
        
            //pridavam uzel na aktualnim indexu
            s.vysl=(1 << s.akt_uz) | s.vysl;
            Stav sx(s.akt_i + 1, s.akt_uz + 1, s.vysl, s.rez);
            
            // nepridavam uzel na aktualnim indexu
            s.vysl=s.vysl & (~(1 << s.akt_uz));

            Stav s2(s.akt_i, s.akt_uz + 1, s.vysl, s.rez);

            ns.push_back(sx);
            ns.push_back(s2);      
            return ns;
    }  
    
    
};



int main(int argc, char** argv) {
    int n, k, a, u1, u2;
    const int tag_work=0;
    const int tag_finished=1;
    const int tag_done=2;
    double v;
    string s;
    int my_rank,num_procs;
    double nej_vysl=INT_MAX;
    uint64_t nej_vector=0;

    int provided, required = MPI_THREAD_FUNNELED;
    MPI_Init_thread(&argc, &argv, required, &provided);
    
    MPI_Comm_rank(MPI_COMM_WORLD , &my_rank );
    MPI_Status status;
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

    if (argv[1]==NULL) s="mhr_30_10_10.txt";
    else s=argv[1];

    ifstream infile (s);

    if (!infile.is_open()){
        cout << "Soubor  nelze nacist: " << s << "\n";
        return 0;
    }
    
    if (!(infile >> n >> k >> a)) {return 1;}
    Graph g(n, a);

    while (infile >> u1 >> u2 >> v){
        g.addEdge(u1,u2,v);
    }
    infile.close();
    
    if (my_rank==0){//master proces
       
        cout << "n: " << n << " k: " << k << " a: " << a << "\n";    
        auto start = std::chrono::high_resolution_clock::now();
        
            queue<Stav> q;
            Stav sa=Stav(0,0,0,0);
            q.push(sa);
            vector<Stav>p;
            //vytvarim frontu stavu
            while (q.size() < 100){
                p=g.mhr_cs(q.front());
                q.pop();
                if(p.size()==2){
                  q.push(p[0]);
                  q.push(p[1]);
                }  
            }

            //pocatecni prideleni prace
            for (int dest = 1; dest < num_procs; dest++){ // pro vsechny slave procesy
                Stav s = q.front();
                MPI_Send(&s, 4, MPI_UINT64_T, dest, tag_work, MPI_COMM_WORLD);
                q.pop();
            }
            int working_slaves = num_procs - 1; // pocet pracujicich slave
            
            while (working_slaves > 0) { //hlavni smycka
                
                Stav vysl;
                MPI_Recv(&vysl, 4, MPI_UINT64_T, MPI_ANY_SOURCE, tag_done, MPI_COMM_WORLD, &status);
                
                if (vysl.rez < nej_vysl){ //aktualizace vysledku
                    nej_vysl=vysl.rez;
                    nej_vector=vysl.vysl;   
                }
                    
                if (!q.empty()){ // existuje dalsi prace, fronta neni prazdna
                        Stav s = q.front();
                        MPI_Send(&s, 4, MPI_UINT64_T, status.MPI_SOURCE, tag_work, MPI_COMM_WORLD); //poslu dalsi praci
                        q.pop();
                }
                else {
                    int i=0;
                        MPI_Send(&i,1,MPI_INT, status.MPI_SOURCE, tag_finished, MPI_COMM_WORLD); //prace je hotova
                        working_slaves--;
                      }
            }
            
            auto finish = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double> elapsed = finish - start;
            g.printRes(nej_vysl, nej_vector);  
            cout << "Cas: " << elapsed.count() << " s\n";
                        
    }
    else{//slave procesy

        while (true) {
            Stav s4;
            MPI_Recv(&s4, 4, MPI_UINT64_T, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
            if (status.MPI_TAG == tag_finished) break; // vypocet konci
            else if (status.MPI_TAG == tag_work) {
                //vypocet minimalniho hranoveho rezu
                #pragma  omp  parallel
                {
                    #pragma  omp  single
                    g.mhrN(s4.akt_i, s4.akt_uz, s4.vysl, s4.rez);
                }
               
                Stav v(0,0,g.best_vector,g.best_sol_number);
                MPI_Send(&v,4,MPI_UINT64_T, 0, tag_done, MPI_COMM_WORLD);
            }
        }
        
    }

    MPI_Finalize();
    return 0;
}



