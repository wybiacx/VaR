#include<cstdio>
#include<ctime>
#include<cstdlib>
#include<iostream>
#include<string>
#include<cstring>
#include<vector>
#include<fstream>
#include<cmath>
#include<algorithm>
using namespace std;

/*--------------parameter----------------*/

#define POPULATION 100
#define N 94
#define GENERATION 500
#define F 0.3
#define CR 0.9
#define A_max 100
#define D_max 10
#define K 10
#define Ei 0.01
#define vi 0.008
#define M 6
#define Lm 0.05
#define Um 0.75

#define T 999 
#define aa 0.05

#define INF 0x3fffff

/*--------------------------------------*/

struct Asset{
	string name;
	double Aver_return, Sta_deviation;
	vector<double> _return;
	void Cal(){
		Aver_return = 0;
		Sta_deviation = 0;
		for(int i = 0; i < _return.size(); i++)
			Aver_return += _return[i];
		Aver_return /= _return.size();
		for(int i = 0; i < _return.size(); i++)
			Sta_deviation += pow((_return[i] - Aver_return), 2);
		Sta_deviation /= _return.size()-1;
		Sta_deviation = sqrt(Sta_deviation); 
	}
};

struct Portfolio{
	vector<int> select_assets;
	vector<double> w;
	double profit,VaR,Crowd; 
	Portfolio(){
		profit = 0;
		VaR = 0;
		Crowd = 0;
	}
	bool operator > (const Portfolio &b){
		if((VaR <= b.VaR && profit > b.profit) || (profit >= b.profit && VaR < b.VaR))
			return true;
		else return false;
	}
};




vector<Asset> Assets;

vector<Portfolio> Portfolios;

vector<Portfolio> A , D;

vector<int> Z;

double C[N+1];


int ID(int now){
	int id = 0;
	for(int i = 1; i <= 6; i++){
		int mod = i == 6 ? 19 : 15;
		if(15*(i-1)+1 <= now && now <= (i-1)*15+mod){
			id = i;
			break;
		}
	}
	return id;
}

void Input(){
	
	ifstream fin(".\\MODE-GL-DataSets\\S&P100\\stocktickerSP94.txt");
	if(!fin.is_open()){
		printf("Data name files open error!\n");
		return;
	}
	
	Asset temp;
	while(getline(fin,temp.name)){
		Assets.push_back(temp);
	}
	fin.close();
	fin.clear();
	
	for(int i = 0; i < Assets.size(); i++){
		
		string file_name = ".\\MODE-GL-DataSets\\S&P100\\" + Assets[i].name + ".csv";
		fin.open(file_name.c_str());
		if(!fin.is_open()){
			printf("Data file open error!\n");
			return;
		}
		
		vector<string> now_data;
		string temp2;
		while(getline(fin,temp2))
			now_data.push_back((temp2));
		fin.close();
		fin.clear();
		vector<double> now_return;
		for(int j = 1; j < now_data.size(); j++){
			double temp3;
			int index1,index2;
			index1 = now_data[j].find(',');
			index2 = now_data[j].find(',',index1+1);
			string sub = now_data[j].substr(index1+1,index2-index1-1);
			sscanf(sub.c_str(),"%lf",&temp3);
			now_return.push_back(temp3);
		}
		for(int j = 0; j < now_return.size()-1; j++)
			now_return[j] -= now_return[j+1];
		now_return.pop_back();
		Assets[i]._return = now_return;
		Assets[i].Cal();
	}
}


void test1(){
	for(int i = 0; i < Assets.size(); i++){
		cout << Assets[i].name << endl;
	}
	
	cout << endl << Assets[50].name << endl;
	cout << "Average return :" << Assets[50].Aver_return << endl;
	cout << "Standard deviation :" << Assets[50].Sta_deviation << endl;
	for(int i = 0; i < Assets[50]._return.size(); i++)
		printf("Day%d:%.2f\n",i+1,Assets[50]._return[i]);
}

double Kt(int t, const Portfolio &now){
	double val = 0;
	for(int i = 0; i < now.select_assets.size(); i++){
		int id = now.select_assets[i];
		double wi = now.w[i];
		val += wi*Assets[id-1]._return[t];
	}
	return val;	
}

void Evaluate(Portfolio &now){
	double pro = 0,Va = 0;
	double Ot = 1.0/T;
	
	vector<double> kts;
	for(int i = 0; i < T; i++){
		double now_kt = Kt(i,now);
		pro += Kt(i,now)*Ot;
		kts.push_back(now_kt);
	}
	
	sort(kts.begin(),kts.end());
	double acumulate = 0;
	for(int i = 0; i < kts.size(); i++){
		acumulate += Ot;
		if(acumulate >= aa){
			Va = -kts[i];
		}
	}	
		
	now.profit = pro;
	now.VaR = Va;
}


void Repair(Portfolio &now){
	
	//first step
	double sum_w = 0,min_lot;
	for(int i = 1;;i++)
		if(i*vi >= Ei){
			min_lot = i*vi;
			break;
		}
	double remain = 1.0 - K*min_lot;
	
	for(int i = 0; i < now.w.size(); i++){
		sum_w += now.w[i];
	}
	
	for(int i = 0; i < now.w.size(); i++)
		now.w[i] = min_lot + (now.w[i] / sum_w) * remain;
		
	//printf("R1\n");
	//second step
	int count = 0,max_index = 0;
	double sum_remainw = 0,max_remain = -10;
	for(int i = 0; i < now.select_assets.size(); i++){
		count = now.w[i] / vi;
		double now_remain = now.w[i] - vi*count;
		now.w[i] = vi*count;
		if(now_remain > max_remain){
			max_remain = now_remain;
			max_index = i;
		}
		sum_remainw += now_remain;
	}
	
	now.w[max_index] += sum_remainw;
	/*-------debug---------------*/
	/*
	double sum_ll = 0;
	for(int i = 0; i < now.select_assets.size();i++){
		printf("%.3f  ",now.w[i]);
		sum_ll += now.w[i];
		int countc = now.w[i] / vi;
		if(countc * vi == now.w[i])
			printf("true\n");
	}
	printf("sum = %.3f\n",sum_ll);
	
	*/
	/*----------------------------*/	
	//printf("R2\n");
	//third step
	double class_w[M+1];
	memset(class_w,0,sizeof(class_w));
	for(int i = 0; i < now.select_assets.size(); i++)
		class_w[ ID(now.select_assets[i])] += now.w[i];
	
	/*-----------------Debug------------*/
	/*for(int i = 0; i < now.select_assets.size(); i++)
		printf("asset %d--->class %d\n",now.select_assets[i],ID(now.select_assets[i]));
	*/
	/*----------------------------------*/
	int stack[N+1],tail;
	for(int i = 1; i <= M; i++)
		while(class_w[i] < Lm){
			
			/*for(int j = 1; j <= M; j++)
				printf("class_w %d = %.3f\n",j,class_w[j]);
			*/
			//printf("0\n");	
			tail = 0;
			for(int j = 1; j <= M; j++)
				if(j == i)continue;
				else if(class_w[j] >= Lm + vi){
					
					for(int t = 0; t < now.select_assets.size();t++)
						if( ID(now.select_assets[t]) == j && now.w[t] >= min_lot + vi)
							stack[++tail] = t;
					if(tail == 0)continue;
					int select_sub = rand() % tail + 1;
					now.w[stack[select_sub]] -= vi;	
					class_w[j] -= vi;
					break;
				}
			
			//printf("a\n");
			//printf("tail = %d\n",tail);
			//printf("b\n");
			tail = 0;
			for(int j = 0; j < now.select_assets.size(); j++)
				if( ID(now.select_assets[j]) == i)	
					stack[++tail] = j;
			int select_add = rand() % tail + 1;
			now.w[stack[select_add]] += vi;
			class_w[i] += vi;
		}
	//printf("R3\n");
} 

bool cmp3(const Portfolio &a,const Portfolio &b){
	return a.profit < b.profit;
}

bool cmp4(const Portfolio &a,const Portfolio &b){
	return a.VaR < b.VaR;
}

void Calculate_Crowd(){
	
	sort(Portfolios.begin(),Portfolios.end(),cmp3);
	
	
	for(int i = 1; i < Portfolios.size()-1; i++){
		double d1 = Portfolios[i].profit - Portfolios[i-1].profit;
		double d2 = Portfolios[i+1].profit - Portfolios[i].profit;
		Portfolios[i].Crowd = (d1+d2)/2;
	}
	Portfolios[0].Crowd = INF;
	Portfolios[Portfolios.size()-1].Crowd = INF;
	
	sort(Portfolios.begin(),Portfolios.end(),cmp4);
	
	for(int i = 1; i < Portfolios.size()-1; i++){
		double d1 = Portfolios[i].VaR - Portfolios[i-1].VaR;
		double d2 = Portfolios[i+1].VaR - Portfolios[i].VaR;
		Portfolios[i].Crowd += (d1+d2)/2;
	}
	Portfolios[0].Crowd = INF;
	Portfolios[Portfolios.size()-1].Crowd = INF;
}
void Initial_Group(){
	Z.push_back(30);
	for(int i = 1; i <= POPULATION; i++){
		Portfolio temp;
		
		int class_use[M+1],asset_use[101],count = 0;
		memset(class_use,0,sizeof(class_use));
		memset(asset_use,0,sizeof(asset_use));
		for(int j = 0; j < Z.size(); j++){
			temp.select_assets.push_back(Z[j]);
			class_use[ID(Z[j])] = 1;
			asset_use[Z[j]] = 1; 
			count++;
		}
				
		//remain K-Z
		for(int j = 1; j <= M; j++)
			if(!class_use[j]){
				int mod = j == 6 ? 19 : 15;
				int rand_s = rand()% mod+1;
				temp.select_assets.push_back(rand_s + (j-1)*15);
				asset_use[rand_s + (j-1)*15] = 1;
				count++;
			}
		
		for(int j = 1; j <= K-count; j++){
			
			int rand_s;
			do{
				rand_s = rand()%94;
			}while(asset_use[rand_s]);
			temp.select_assets.push_back(rand_s);
			asset_use[rand_s] = 1;
		}
		
		
		//allocate proportion
		double fir = 0,remain;
		int remain_count = 0,sum_rand = 0;
		vector<int> rand_i;
		for(int j = 1; ; j++)
			if(vi*j >= Ei){
				fir = vi*j;
				break;
			}
		remain = 1.0 - K*fir;
		remain_count = remain / vi;
		for(int j = 0; j < temp.select_assets.size(); j++)
			temp.w.push_back(fir);
		for(int j = 0; j < temp.select_assets.size(); j++){
			int now_rand = rand()%100;
			sum_rand += now_rand;
			rand_i.push_back(now_rand);
		}
		for(int j = 0; j < temp.select_assets.size()-1; j++){
			temp.w[j] +=  (rand_i[j]/sum_rand)*remain_count  * 1.0*vi;
			remain -= (rand_i[j]/sum_rand)*remain_count  * 1.0*vi;
		}
		temp.w[temp.select_assets.size()-1] += remain;
			
		Evaluate(temp);
		Portfolios.push_back(temp);
	}
	
	Calculate_Crowd();
} 

bool cmp1(const Portfolio &a, const Portfolio &b){
	if((a.VaR <= b.VaR && a.profit > b.profit) || (a.profit >= b.profit && a.VaR < b.VaR))
		return true;
	else return false;

}

void Maintain_A(){
	
	for(int i = 0; i < Portfolios.size();i++)
		A.push_back(Portfolios[i]);
	
	sort(A.begin(),A.end(),cmp1);
	
	while(A.size() > A_max)
		A.pop_back();
}

bool cmp2(const Portfolio &a, const Portfolio &b){
	return a.Crowd > b.Crowd;
}

void Maintain_D(){
	
	for(int i = 0; i < Portfolios.size(); i++)
		D.push_back(Portfolios[i]);
	
	sort(D.begin(),D.end(),cmp2);
		
	while(D.size() > D_max)
		D.pop_back();
}

void Calculate_C(){
	
	memset(C,0,sizeof(C));
	for(int i = 0; i < A.size(); i++)
		for(int j = 0; j < A[i].select_assets.size(); j++)
			C[A[i].select_assets[j]]++;
	
	for(int i = 1; i <= Assets.size(); i++)
		C[i] /= A.size();
	
}



/*------------------------New S1~S4--------------------------*/

int S1(int use_asset[],int l,int r){
	double sum_c = 0,temp_c = 0,rand_cr;
	vector<int> sta;
	for(int i = l; i <= r; i++)
		if(!use_asset[i]){
			sum_c += C[i];
			sta.push_back(i);
		}
	
	int index = l;
	rand_cr = rand()%99+1;
	rand_cr /= 100;
	for(int i = 0; i < sta.size(); i++){
		temp_c += C[sta[i]] / sum_c;
		if(rand_cr <= temp_c){
			index = sta[i];
			break;
		}
	}
	return index;
}

int S2(int use_asset[],int l,int r){
	double max_c = -1;
	int index = 0;
	for(int i = l; i <= r; i++)
		if(!use_asset[i]){
			if(C[i] > max_c){
				max_c = C[i];
				index = i;
			}
		}
	return index;
}

int S3(int use_asset[], int l,int r){
	double max_mean_return = -1;
	int index = 0;
	for(int i = l; i <= r; i++)
		if(!use_asset[i]){
			if(Assets[i-1].Aver_return > max_mean_return){
				index = i;
				max_mean_return = Assets[i-1].Aver_return;
			}
		}
	return index;
}

int S4(int use_asset[],int l,int r){
	double min_sta_de = 9999;
	int index = 0;
	for(int i = l; i <= r; i++)
		if(!use_asset[i]){
			if(Assets[i-1].Sta_deviation < min_sta_de){
				min_sta_de = Assets[i-1].Sta_deviation;
				index = i;
			}
		}
	return index;
}
/*--------------------------------------------------------*/
bool False(Portfolio &now){
	int class_use[M+1];
	memset(class_use,0,sizeof(class_use));
	for(int i = 0; i < now.select_assets.size(); i++)
		class_use[ ID(now.select_assets[i])] = 1;
	
	int flag = 1;
	for(int i = 1; i<= M; i++)
		if(!class_use[i])flag = 0;
	return !flag;
}

Portfolio Generate(const int index_p){
	
	Portfolio temp;
	
	//select assets
	
	int use_asset[N+1],use_class[M+1];
	memset(use_asset, 0, sizeof(use_asset));
	memset(use_class, 0, sizeof(use_class));
	//pre	Z
	for(int i = 0; i < Z.size(); i++){
		temp.select_assets.push_back(Z[i]);
		use_asset[Z[i]] = 1;
		use_class[ ID(Z[i])] = 1;
	}
		
	
	for(int i = 1; i <= M; i++)
		if(!use_class[i]){
			int rand_s = rand() % 4 + 1;
			int mod = i == 6 ? 19 : 15;
			int l = (i-1)*15 + 1;
			int r = (i-1)*15 + mod;
			int index;
			//printf("rand_s = %d   l = %d   r = %d\n",rand_s,l,r);
			if(rand_s == 1){
				index = S1(use_asset,l,r);
			} 
			else if(rand_s == 2){
				index = S2(use_asset,l,r);
			}
			else if(rand_s == 3){
				index = S3(use_asset,l,r);
			}
			else{
				index = S4(use_asset,l,r);
			}
			temp.select_assets.push_back(index);
			use_asset[index] = 1;
		}
	
	
	
	for(int i = 1; i <= K-M; i++){
		
		int rand_s = rand() % 4 + 1;
		int index;
		
		if(rand_s == 1){
			index = S1(use_asset,1,N);
		}
		else if(rand_s == 2){
			index = S2(use_asset,1,N);
		}
		else if(rand_s == 3){
			index = S3(use_asset,1,N);
		}
		else{
			index = S4(use_asset,1,N);
		}
		
		temp.select_assets.push_back(index);
		use_asset[index] = 1;
	}
	
	/*
	for(int i = 1; i <= K-Z.size(); i++){
		int r = rand()%4 + 1;
		int select;
		
		if(r == 1){
			select = S1(use_asset);
		}
		else if(r == 2){
			select = S2(use_asset);
		}
		else if(r == 3){
			select = S3(use_asset);
		}
		else{
			select = S4(use_asset);
		}
		
		use_asset[select] = 1;
		temp.select_assets.push_back(select);
	}
	*/
	
	
	/*------------Debug---------------*/
	/*if(False(temp))printf("error!\n");
	else printf("True\n");
	/*--------------------------------*/
	
	Portfolio best,p1,p2;
	
	do{
		int r = rand() % D.size();
		p1 = D[r];
	}while(p1 == Pa);
	
	do{
		int r = rand() % D.size();
		p2 = D[r];
	}while(p2 == p1 || p2 == Pa);
	
	do{
		int r = rand() % (A.size()/10);
		best = A[r];
	}while(best == Pa || best == p1 || best == p2);
	
	
	int i,j,y;
	i = rand() % K;
	j = y = i;
	
	for(i = 0; i < temp.select_assets.size();i++){
		double r = rand()%99 + 1;
		r = r / 100;
		if(r < CR || j == y){
			int r1 = rand()%2 + 1;
			if(r1 == 1){
				double r = rand()%99 + 1;
				r /= 100; 
				double now_w = best.w[i] + r * (p1.w[i] - p2.w[i]);
				temp.w.push_back(now_w);
			}
			else{
				double now_w;
				now_w = Pa.w[i] + F*(best.w[i] -Pa.w[i]) + F*(p1.w[i] - p2.w[i]);
				temp.w.push_back(now_w);
			}
		}
		else{
			temp.w.push_back(Pa.w[i]);
		}
		j = rand() % K;
	}
	return temp;
}

bool Violated(const Portfolio &now){
	
	double class_w[M+1];
	memset(class_w,0,sizeof(class_w));
	for(int i = 0; i < now.w.size(); i++){
		class_w[ ID(now.select_assets[i])] += now.w[i];
		int count = now.w[i] / vi;
		if(now.w[i] - vi*count != 0)
			return true;
	}
	
	for(int i = 1; i <= M; i++)
		if(class_w[i] < Lm || class_w[i] > Um)
			return true;
			
	double sum_w = 0;
	for(int i = 1; i <= M; i++)
		sum_w += class_w[i];
	if(sum_w != 1)
		return true;
	
	return false; 
}

void Output(){
	
	for(int i = 0; i < A.size(); i++){
		cout << "#" << i+1 << ":"<<endl;
		printf("Profit: %.6f\nVaR:%.6f\n",A[i].profit,A[i].VaR);
	}
	
}



void MODE_GL(){
	
	int iter = 1;
	Initial_Group();
	
	while(iter <= GENERATION){
		
		Maintain_A();
		Maintain_D();
		Calculate_C();
		
		for(int i = 0; i < Portfolios.size(); i++){
			Portfolio new_p = Generate(i);
			
			if(Violated(new_p))
				Repair(new_p);
				
			Evaluate(new_p);
			
			//overload operator ">" in struct Portfolio
			if(new_p > Portfolios[i])
				Portfolios[i] = new_p;
			else if(Portfolios[i] > new_p){
				//delete new_p;
			}
			else 
				Portfolios.push_back(new_p);
		}
		Calculate_Crowd();
		if(Portfolios.size() > POPULATION){
			sort(Portfolios.begin(), Portfolios.end(),cmp1);
			while(Portfolios.size() > POPULATION)
				Portfolios.pop_back();
		}
		iter++;
	}
}


int main(){
	srand(time(0));
	time_t T_begin = clock();
	
	Input();
	
	test1();
	
	//MODE_GL();
	
	//Output();
	
	time_t T_end = clock();
	double Running_time = (T_end - T_begin) / CLOCKS_PER_SEC;
	printf("Running time:%.2f\n",Running_time);
	return 0;
} 
