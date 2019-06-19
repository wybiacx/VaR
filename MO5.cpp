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
#define GENERATION 5000*2
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

#define INF 99999999

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
	bool operator == (const Portfolio &b){
		if(select_assets == b.select_assets && w == b.w)
			return true;
		else return false;
	}
};




vector<Asset> Assets;

Portfolio Portfolios[2*POPULATION + 10];

Portfolio A[A_max+POPULATION + 10] , D[POPULATION +D_max + 10];

Portfolio Temp_p[2*POPULATION+10]; 

int HEAD_P = 0,HEAD_A = 0,HEAD_D = 0;

int level[2*POPULATION+10],use_A[2*POPULATION+10];

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
		for(int j = 0; j < now_return.size()-1; j++){
			now_return[j] = (now_return[j]-now_return[j+1])/now_return[j+1];
		}
			
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

void display2(Portfolio now){
	
	for(int i = 0; i < now.select_assets.size(); i++)
		printf("%d ",now.select_assets[i]);
	printf("\n");
	for(int i = 0; i < now.select_assets.size(); i++)
		printf("%.6f ",now.w[i]);
	printf("\n");
}
void Evaluate(Portfolio &now){
	double pro = 0,Va = 0;
	double Ot = 1.0/Assets[0]._return.size();
	
	vector<double> kts;
	for(int i = 0; i < Assets[0]._return.size(); i++){
		double now_kt = Kt(i,now);
		pro += now_kt*Ot;
		kts.push_back(now_kt);
	}
	
	sort(kts.begin(),kts.end());
	double acumulate = 0;
	for(int i = 0; i < kts.size(); i++){
		acumulate += Ot;
		if(acumulate >= aa){
			Va = -kts[i];
			break;
		}
	}	
	
//	if(pro != pro){
//		printf("error!\n");
//		display2(now);
//	}
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
	
	double min = 9999;
	
	for(int i = 0; i < now.w.size(); i++)
		if(now.w[i] < min)min = now.w[i];
	
	if(min < 0){
		for(int i = 0; i < now.w.size(); i++)
			now.w[i] += fabs(min);
	}
	
	for(int i = 0; i < now.w.size(); i++){
		sum_w += now.w[i];
	}
	
	for(int i = 0; i < now.w.size(); i++)
		now.w[i] = min_lot + (now.w[i] / sum_w) * remain;
		
	//second step
	int count = 0,max_index = 0;
	double sum_remainw = 0,max_remain = -1;
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

	//third step
	double class_w[M+1];
	memset(class_w,0,sizeof(class_w));
	for(int i = 0; i < now.select_assets.size(); i++)
		class_w[ ID(now.select_assets[i])] += now.w[i];
	
	
	int stack[N+1],tail;
	for(int i = 1; i <= M; i++)
		while(class_w[i] < Lm){
		
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
			
		
			tail = 0;
			for(int j = 0; j < now.select_assets.size(); j++)
				if( ID(now.select_assets[j]) == i)	
					stack[++tail] = j;
			int select_add = rand() % tail + 1;
			now.w[stack[select_add]] += vi;
			class_w[i] += vi;
		}
} 

bool cmp3(const Portfolio &a,const Portfolio &b){
	return a.profit < b.profit;
}

bool cmp4(const Portfolio &a,const Portfolio &b){
	return a.VaR < b.VaR;
}

void Calculate_Crowd(){
	
	sort(Portfolios,Portfolios+HEAD_P,cmp3);
	
	
	for(int i = 1; i < HEAD_P-1; i++){
		double d1 = Portfolios[i].profit - Portfolios[i-1].profit;
		double d2 = Portfolios[i+1].profit - Portfolios[i].profit;
		Portfolios[i].Crowd = (fabs(d1)+ fabs(d2))/2;
	}
	Portfolios[0].Crowd = INF;
	Portfolios[HEAD_P-1].Crowd = INF;
	
	sort(Portfolios,Portfolios+HEAD_P,cmp4);
	
	for(int i = 1; i < HEAD_P-1; i++){
		double d1 = Portfolios[i].VaR - Portfolios[i-1].VaR;
		double d2 = Portfolios[i+1].VaR - Portfolios[i].VaR;
		Portfolios[i].Crowd += (fabs(d1)+ fabs(d2))/2;
	}
	Portfolios[0].Crowd += INF;
	Portfolios[HEAD_P-1].Crowd += INF;
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
				rand_s = rand()%94+1;
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
			int now_rand = rand()%99 + 1;
			sum_rand += now_rand;
			rand_i.push_back(now_rand);
		}
		for(int j = 0; j < temp.select_assets.size()-1; j++){
			temp.w[j] +=  (rand_i[j]/sum_rand)*remain_count  * 1.0*vi;
			remain -= (rand_i[j]/sum_rand)*remain_count  * 1.0*vi;
		}
		temp.w[temp.select_assets.size()-1] += remain;
		Repair(temp);	
		Evaluate(temp);
		Portfolios[HEAD_P++] = temp;
	}
	
	Calculate_Crowd();
} 

//int cmp1(const Portfolio &a, const Portfolio &b){
//	if (a.VaR<b.VaR&&a.profit>b.profit)
//	return 1;
//	if (a.VaR>b.VaR&&a.profit<b.profit)
//	return -1;
//	else
//	return 0; 
//}


void Maintain_A(){
	
	int size_t = 0;
//	printf("1\n");
	memset(use_A,0,sizeof(use_A));
	for(int i = 0; i < HEAD_P; i++)
		A[HEAD_A++] = Portfolios[i];
	for(int i = 0; i < HEAD_A; i++){
		if(!use_A[i]){
			level[size_t] = 1;
			Temp_p[size_t++] = A[i];
		}
		for(int j = i+1; j < HEAD_A; j++)
			if(A[j] == A[i])use_A[j] = 1;
	}
//	printf("2\n");
	HEAD_A = 0;
	int now_level = 1;
	if(size_t < A_max){
		for(int i = 0; i < size_t; i++)
			A[HEAD_A++] = Temp_p[i];
	}
	else while(HEAD_A < A_max){
		now_level++;
		for(int i = 0; i < size_t; i++){
			if(level[i] != now_level-1)continue;
			for(int j = 0; j < size_t;j++){
				if(i == j)continue;
				if(level[j] == now_level-1){
					if((Temp_p[j].VaR <= Temp_p[i].VaR && Temp_p[j].profit > Temp_p[i].profit) ||
						(Temp_p[j].VaR < Temp_p[i].VaR && Temp_p[j].profit >= Temp_p[i].profit)){
							level[i]++;
							break;
						}
				}
			}
		}
		for(int i = 0; i < size_t && HEAD_A < A_max ; i++)
			if(level[i] == now_level-1){
				A[HEAD_A++] = Temp_p[i];
			}	
	}
	
	
}

bool cmp2(const Portfolio &a, const Portfolio &b){
	return a.Crowd > b.Crowd;
}

void Maintain_D(){
	HEAD_D = 0;
//	printf("D1\n");
	for(int i = 0; i < HEAD_P; i++)
		D[HEAD_D++] = Portfolios[i];
//	printf("D2\n");
	sort(D,D+HEAD_D,cmp2);
		
	while(HEAD_D > D_max)
		HEAD_D--;
}

void Calculate_C(){
	
	memset(C,0,sizeof(C));
	for(int i = 0; i < HEAD_A; i++)
		for(int j = 0; j < A[i].select_assets.size(); j++)
			C[A[i].select_assets[j]]++;
	
	for(int i = 1; i <= Assets.size(); i++)
		C[i] /= HEAD_A;
	
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
	double max_c = -20;
	int index = l;
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
	double max_mean_return = -999;
	int index = l;
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
	int index = l;
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
			int mod = (i == 6 )? 19 : 15;
			int l = (i-1)*15 + 1;
			int r = (i-1)*15 + mod;
			int index;
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
	
//	printf("G1\n");
	
	int best,p1,p2;
	
	do{
		best = rand()%(A_max/10);
	}while(A[best] == Portfolios[index_p]);
	
	
	
	do{
		p1 = rand()%D_max;
	}while(D[p1] == A[best] || D[p1] == Portfolios[index_p]);
	
	do{
		p2 = rand()%D_max;	
	}while(p1 == p2 || D[p2] == A[best] || D[p2] == Portfolios[index_p]);
	
	
	
//	printf("G2\n");
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
				double now_w = A[best].w[i] + r * (D[p1].w[i] - D[p2].w[i]);
				temp.w.push_back(now_w);
			}
			else{
				double now_w;
				now_w = Portfolios[index_p].w[i] + F*(A[best].w[i] -Portfolios[index_p].w[i]) + F*(D[p1].w[i] - D[p2].w[i]);
				temp.w.push_back(now_w);
			}
		}
		else{
			temp.w.push_back(Portfolios[index_p].w[i]);
		}
		j = rand() % K;
	}
	return temp;
}


bool Violated(const Portfolio &now){
	
	double class_w[M+1];
	memset(class_w,0,sizeof(class_w));
	for(int i = 0; i < now.select_assets.size(); i++){
		class_w[ ID(now.select_assets[i])] += now.w[i];
		int count = now.w[i] / vi;
		if(now.w[i] < Ei)return true;
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
	
	for(int i = 0; i < HEAD_A; i++){
		printf("%.6f,%.6f\n",A[i].VaR,A[i].profit);
	}
	
}

bool cmp5(const Portfolio &a,const Portfolio &b){
	if((a.VaR <= b.VaR && a.profit > b.profit) || (a.profit >= b.profit && a.VaR < b.VaR))
		return true;
	else if((b.VaR <= a.VaR && b.profit > a.profit) || (b.profit >= a.profit && b.VaR < a.VaR))
		return false;
	else {
		if(a.Crowd > b.Crowd)return true;
		else return false;
	}
}

void display(){
	for(int i = 0; i < HEAD_P; i++){
		for(int j = 0; j < K; j++)
			printf("%d ",Portfolios[i].select_assets[j]);
		printf("\n");
		for(int j = 0; j < K; j++)
			printf("%.2f ",Portfolios[i].w[j]);
		printf("\n");
	}
//	return;
	for(int i = 0; i < HEAD_P; i++)
		printf("VaR = %.6f,pro = %.6f\n",Portfolios[i].VaR,Portfolios[i].profit);
}

void MODE_GL(){
	
	int iter = 1;
	Initial_Group();
	while(iter <= GENERATION){
		//printf("1\n");
		Maintain_A();
		//printf("A\n");
		Maintain_D();
		//printf("D\n");
		Calculate_C();
		//printf("2\n");
		for(int i = 0; i < POPULATION; i++){
			Portfolio new_p = Generate(i);
			//printf("3\n");
			if(Violated(new_p))
				Repair(new_p);
			//printf("4\n");
			Evaluate(new_p);
			//printf("5\n");
			//overload operator ">" in struct Portfolio
			if(new_p > Portfolios[i])
				Portfolios[i] = new_p;
			else if(Portfolios[i] > new_p){
				//delete new_p;
			}
			else 
				Portfolios[HEAD_P++] = new_p;
		}
		//printf("6\n");
		Calculate_Crowd();
		if(HEAD_P > POPULATION){
			sort(Portfolios, Portfolios+HEAD_P,cmp5);
			while(HEAD_P >= POPULATION)
				HEAD_P--;
		}
		//printf("7\n");
		iter++;
		printf("iter = %d\n",iter);
	}
}


int main(){
//	freopen("tenth_out.csv","w",stdout);
	srand(time(0));
	time_t T_begin = clock();
	
	Input();
	
	//test1();
	
	MODE_GL();
	
	Output();
	
	time_t T_end = clock();
	double Running_time = (T_end - T_begin) / CLOCKS_PER_SEC;
	printf("Running time:%.2f\n",Running_time);
//	fclose(stdout);
	return 0;
} 
