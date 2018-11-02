library(ggplot2)
library(pomp)
library(readr)
library(dplyr)
library(knitr)

# read in data
sj_dengue = read_csv('http://dengueforecasting.noaa.gov/Training/San_Juan_Training_Data.csv')
sj_dengue = sj_dengue %>% mutate(total_other = other_positive_cases + additional_cases, week = 0:(nrow(sj_dengue)-1))
for(i in 7:nrow(sj_dengue)){
  unassigned = sj_dengue$total_other[i]
  sum_prev5_weeks = sj_dengue[c(
    'denv1_cases', 'denv2_cases','denv3_cases','denv4_cases')][(i-5):(i-1),] %>%
    apply(2, sum)
  assignment = rmultinom(1, unassigned, sum_prev5_weeks)
  sj_dengue[i,]$denv1_cases = sj_dengue[i,]$denv1_cases + assignment['denv1_cases', 1]
  sj_dengue[i,]$denv2_cases = sj_dengue[i,]$denv2_cases + assignment['denv2_cases', 1]
  sj_dengue[i,]$denv3_cases = sj_dengue[i,]$denv3_cases + assignment['denv3_cases', 1]
  sj_dengue[i,]$denv4_cases = sj_dengue[i,]$denv4_cases + assignment['denv4_cases', 1]
}
dd = sj_dengue %>% select(week, y1 = denv1_cases, y2 = denv2_cases, y3 = denv3_cases, y4 = denv4_cases)
dd_test = dd[1:2,]

# create pomp pieces
dengue_skel <- Csnippet('
                        int numstrains = 4;
                        // initialize all the phis and gammas
                        double phi12, phi13, phi14, phi21, phi23, phi24, phi31, phi32, phi34, phi41, phi42, phi43;
                        phi12 = phi13 = phi14 = phi21 = phi23 = phi24 = phi31 = phi32 = phi34 = phi41 = phi42 = phi43 = phi;
                        double gamma12, gamma13, gamma14, gamma21, gamma23, gamma24, gamma31, gamma32, gamma34, gamma41, gamma42, gamma43;
                        gamma12 = gamma13 = gamma14 = gamma21 = gamma23 = gamma24 = gamma31 = gamma32 = gamma34 = gamma41 = gamma42 = gamma43 = Gamma;
                        // for now mu, sigma and bet are fixed
                        //double mu = 1/(70*365);
                        //double sigma = 1/3.65;
                        //double bet = 400/365;
                        double phis[4][4] = {{0, phi12, phi13, phi14}, {phi21, 0, phi23, phi24}, {phi31, phi32, 0, phi34}, {phi41, phi42, phi43, 0}};
                        double n_sec[4][4] = {{0, n12, n13, n14}, {n21, 0, n23, n24}, {n31, n32, 0, n34}, {n41, n42, n43, 0}};
                        double gammas[4][4] = {{0, gamma12, gamma13, gamma14}, {gamma21, 0, gamma23, gamma24}, {gamma31, gamma32, 0, gamma34}, {gamma41, gamma42, gamma43, 0}};
                        double lambdas[4] = {0, 0, 0, 0};
                        double n[4] = {n1, n2, n3, n4};
                        double rs[4] = {r1, r2, r3, r4};
                        double deltas[10];
                        double deltas_sec[4][4];
                        double sum_lambdas = 0;
                        double sumprod = 0;
                        double sumprodtwo = 0;
                        for(int i = 0; i < numstrains; i++){
                        sumprod = 0;
                        for(int j = 0; j < numstrains; j++){
                        if(i == j){
                        sumprod += 0;
                        }
                        else{
                        sumprod += phis[j][i]*n_sec[j][i];
                        }
                        }
                        lambdas[i] = bet*(n[i] + sumprod);
                        sum_lambdas += lambdas[i];
                        }
                        for(int i = 0; i < numstrains; i++){
                        sumprodtwo = 0;
                        for(int j = 0; j < numstrains; j++){
                        if(i != j){
                        sumprodtwo += gammas[i][j]*lambdas[j];
                        }
                        }
                        deltas[i+5] = sigma*n[i] - (rs[i]*(mu + sumprodtwo));
                        }
                        deltas[0] = mu - s*sum_lambdas - mu*s;
                        for(int i = 0; i<numstrains; i++){
                        deltas[i+1] = s*lambdas[i] - (sigma + mu)*n[i];
                        }
                        double sum_ys = 0;
                        for(int i = 0; i<numstrains; i++){
                        for(int j = 0; j<numstrains; j++){
                        if(i == j) {
                        deltas_sec[i][j] = 0;
                        }
                        else{
                        deltas_sec[i][j] = rs[i]*gammas[i][j]*lambdas[j] - (sigma + mu)*n_sec[i][j];
                        sum_ys += n_sec[i][j];
                        }
                        }
                        }
                        deltas[9] = sigma*sum_ys - mu*r;
                        Ds = (s + deltas[0]);
                        Dn1 = (n1 + deltas[1]);
                        Dn2 = (n2 + deltas[2]);
                        Dn3 = (n3 + deltas[3]);
                        Dn4 = (n4 + deltas[4]);
                        Dr1 = (r1 + deltas[5]);
                        Dr2 = (r2 + deltas[6]);
                        Dr3 = (r3 + deltas[7]);
                        Dr4 = (r4 + deltas[8]);
                        Dr = (r + deltas[9]);
                        Dn12 = (n12 + deltas_sec[0][1]);
                        Dn13 = (n13 + deltas_sec[0][2]);
                        Dn14 = (n14 + deltas_sec[0][3]);
                        Dn21 = (n21 + deltas_sec[1][0]);
                        Dn23 = (n23 + deltas_sec[1][2]);
                        Dn24 = (n24 + deltas_sec[1][3]);
                        Dn31 = (n31 + deltas_sec[2][0]);
                        Dn32 = (n32 + deltas_sec[2][1]);
                        Dn34 = (n34 + deltas_sec[2][3]);
                        Dn41 = (n41 + deltas_sec[3][0]);
                        Dn42 = (n42 + deltas_sec[3][1]);
                        Dn43 = (n43 + deltas_sec[3][2]);
                        ')

dengue_rprocess <- Csnippet('
                            int numstrains = 4;
                            int population = 2217968;
                            
                            // initialize all the phis and gammas
                            double phi12, phi13, phi14, phi21, phi23, phi24, phi31, phi32, phi34, phi41, phi42, phi43;
                            phi12 = phi13 = phi14 = phi21 = phi23 = phi24 = phi31 = phi32 = phi34 = phi41 = phi42 = phi43 = phi;
                            double gamma12, gamma13, gamma14, gamma21, gamma23, gamma24, gamma31, gamma32, gamma34, gamma41, gamma42, gamma43;
                            gamma12 = gamma13 = gamma14 = gamma21 = gamma23 = gamma24 = gamma31 = gamma32 = gamma34 = gamma41 = gamma42 = gamma43 = Gamma;
                            
                            // useful for calculations
                            double phis[4][4] = {{0, phi12, phi13, phi14}, {phi21, 0, phi23, phi24}, {phi31, phi32, 0, phi34}, {phi41, phi42, phi43, 0}};
                            double n_sec[4][4] = {{0, n12, n13, n14}, {n21, 0, n23, n24}, {n31, n32, 0, n34}, {n41, n42, n43, 0}};
                            double lambdas[4] = {0, 0, 0, 0};
                            double n[4] = {n1, n2, n3, n4};
                            double sumprod = 0;
                            double s_rate[5], s_trans[5], p_inf_rate[8], p_inf_trans[8], r_rate[16], r_trans[16], s_inf_rate[24], s_inf_trans[24], rec_rate[1], rec_trans[1];
                            int sum_deaths = 0;
                            int births;
                            
                            // calculate foi for each strain
                            for(int i = 0; i < numstrains; i++){
                            sumprod = 0;
                            for(int j = 0; j < numstrains; j++){
                            if(i == j){
                            sumprod += 0;
                            }
                            else{
                            sumprod += phis[j][i]*n_sec[j][i];
                            }
                            }
                            lambdas[i] = bet*(n[i] + sumprod);
                            }
                            
                            // rates from S to primary I
                            s_rate[0] = lambdas[0]; // to I1
                            s_rate[1] = lambdas[1]; // to I2
                            s_rate[2] = lambdas[2]; // to I3
                            s_rate[3] = lambdas[3]; // to I4
                            s_rate[4] = mu; // natural death
                            
                            // transitions from S to primary I
                            reulermultinom(5, floor(s*population), &s_rate[0], 1, &s_trans[0]);
                            //printf("%f",floor(s*population));
                            //printf(" ");
                            //printf("%f", s_trans[0]);
                            //printf(" ");
                            //printf("%f", s_trans[1]);
                            //printf(" ");
                            //printf("%f", s_trans[2]);
                            //printf(" ");
                            //printf("%f", s_trans[3]);
                            //printf(" ");
                            //printf("%f", s_trans[4]);
                            //printf("End One ");
                            
                            // rates from I_i to R_i and death
                            p_inf_rate[0] = sigma;
                            p_inf_rate[1] = mu;
                            p_inf_rate[2] = sigma;
                            p_inf_rate[3] = mu;
                            p_inf_rate[4] = sigma;
                            p_inf_rate[5] = mu;
                            p_inf_rate[6] = sigma;
                            p_inf_rate[7] = mu;
                            
                            // transitions from I_i to R_i and death
                            reulermultinom(2, floor(n[0]*population), &p_inf_rate[0], 1, &p_inf_trans[0]);
                            reulermultinom(2, floor(n[1]*population), &p_inf_rate[2], 1, &p_inf_trans[2]);
                            reulermultinom(2, floor(n[2]*population), &p_inf_rate[4], 1, &p_inf_trans[4]);
                            reulermultinom(2, floor(n[3]*population), &p_inf_rate[6], 1, &p_inf_trans[6]);
                            
                            // rates from R_i to I_ij and death
                            r_rate[0] = gamma12*lambdas[1];
                            r_rate[1] = gamma13*lambdas[2];
                            r_rate[2] = gamma14*lambdas[3];
                            r_rate[3] = mu;
                            r_rate[4] = gamma21*lambdas[0];
                            r_rate[5] = gamma23*lambdas[2];
                            r_rate[6] = gamma24*lambdas[3];
                            r_rate[7] = mu;
                            r_rate[8] = gamma31*lambdas[0];
                            r_rate[9] = gamma32*lambdas[1];
                            r_rate[10] = gamma34*lambdas[3];
                            r_rate[11] = mu;
                            r_rate[12] = gamma41*lambdas[0];
                            r_rate[13] = gamma42*lambdas[1];
                            r_rate[14] = gamma43*lambdas[2];
                            r_rate[15] = mu;
                            
                            // transitions from R_i to I_ij and death
                            reulermultinom(4, floor(r1*population), &r_rate[0], 1, &r_trans[0]);
                            reulermultinom(4, floor(r2*population), &r_rate[4], 1, &r_trans[4]);
                            reulermultinom(4, floor(r3*population), &r_rate[8], 1, &r_trans[8]);
                            reulermultinom(4, floor(r4*population), &r_rate[12], 1, &r_trans[12]);
                            
                            // rates from I_ij to R
                            s_inf_rate[0] = sigma;
                            s_inf_rate[1] = mu;
                            s_inf_rate[2] = sigma;
                            s_inf_rate[3] = mu;
                            s_inf_rate[4] = sigma;
                            s_inf_rate[5] = mu;
                            s_inf_rate[6] = sigma;
                            s_inf_rate[7] = mu;
                            s_inf_rate[8] = sigma;
                            s_inf_rate[9] = mu;
                            s_inf_rate[10] = sigma;
                            s_inf_rate[11] = mu;
                            s_inf_rate[12] = sigma;
                            s_inf_rate[13] = mu;
                            s_inf_rate[14] = sigma;
                            s_inf_rate[15] = mu;
                            s_inf_rate[16] = sigma;
                            s_inf_rate[17] = mu;
                            s_inf_rate[18] = sigma;
                            s_inf_rate[19] = mu;
                            s_inf_rate[20] = sigma;
                            s_inf_rate[21] = mu;
                            s_inf_rate[22] = sigma;
                            s_inf_rate[23] = mu;
                            
                            // transitions from secondary infection to permanent recovery
                            reulermultinom(2, floor(n12*population), &s_inf_rate[0], 1, &s_inf_trans[0]);
                            reulermultinom(2, floor(n13*population), &s_inf_rate[2], 1, &s_inf_trans[2]);                        
                            reulermultinom(2, floor(n14*population), &s_inf_rate[4], 1, &s_inf_trans[4]);
                            reulermultinom(2, floor(n21*population), &s_inf_rate[6], 1, &s_inf_trans[6]);
                            reulermultinom(2, floor(n23*population), &s_inf_rate[8], 1, &s_inf_trans[8]);
                            reulermultinom(2, floor(n24*population), &s_inf_rate[10], 1, &s_inf_trans[10]);
                            reulermultinom(2, floor(n31*population), &s_inf_rate[12], 1, &s_inf_trans[12]);
                            reulermultinom(2, floor(n32*population), &s_inf_rate[14], 1, &s_inf_trans[14]);
                            reulermultinom(2, floor(n34*population), &s_inf_rate[16], 1, &s_inf_trans[16]);
                            reulermultinom(2, floor(n41*population), &s_inf_rate[18], 1, &s_inf_trans[18]);
                            reulermultinom(2, floor(n42*population), &s_inf_rate[20], 1, &s_inf_trans[20]);
                            reulermultinom(2, floor(n43*population), &s_inf_rate[22], 1, &s_inf_trans[22]);
                            
                            // rate from permanent recovery to death
                            rec_rate[0] = mu;
                            
                            // transitions from permanent recovery to death
                            reulermultinom(1, floor(r*population), &rec_rate[0], 1, &rec_trans[0]);
                            
                            // updates
                            // add up natural deaths from each box
                            sum_deaths = s_trans[4] + p_inf_trans[1] + p_inf_trans[3] + p_inf_trans[5] + p_inf_trans[7] + r_trans[3] + r_trans[7] + r_trans[11] + r_trans[15] + s_inf_trans[1] + s_inf_trans[3] + s_inf_trans[5] + s_inf_trans[7] + s_inf_trans[9] + s_inf_trans[11] + s_inf_trans[13] + s_inf_trans[15] + s_inf_trans[17] + s_inf_trans[19] + s_inf_trans[21] + s_inf_trans[23] + rec_trans[0];
                            births = sum_deaths;
                            
                            // updates
                            s += (births - s_trans[0] - s_trans[1] - s_trans[2] - s_trans[3] - s_trans[4])/population;
                            n1 += (s_trans[0] - p_inf_trans[0] - p_inf_trans[1])/population;
                            n2 += (s_trans[1] - p_inf_trans[2] - p_inf_trans[3])/population;
                            n3 += (s_trans[2] - p_inf_trans[4] - p_inf_trans[5])/population;
                            n4 += (s_trans[3] - p_inf_trans[6] - p_inf_trans[7])/population;
                            r1 += (p_inf_trans[0] - r_trans[0] - r_trans[1] - r_trans[2] - r_trans[3])/population;
                            r2 += (p_inf_trans[2] - r_trans[4] - r_trans[5] - r_trans[6] - r_trans[7])/population;
                            r3 += (p_inf_trans[4] - r_trans[8] - r_trans[9] - r_trans[10] - r_trans[11])/population;
                            r4 += (p_inf_trans[6] - r_trans[12] - r_trans[13] - r_trans[14] - r_trans[15])/population;
                            n12 += (r_trans[0] - s_inf_trans[0] - s_inf_trans[1])/population;
                            n13 += (r_trans[1] - s_inf_trans[2] - s_inf_trans[3])/population;
                            n14 += (r_trans[2] - s_inf_trans[4] - s_inf_trans[5])/population;
                            n21 += (r_trans[4] - s_inf_trans[6] - s_inf_trans[7])/population;
                            n23 += (r_trans[5] - s_inf_trans[8] - s_inf_trans[9])/population;
                            n24 += (r_trans[6] - s_inf_trans[10] - s_inf_trans[11])/population;
                            n31 += (r_trans[8] - s_inf_trans[12] - s_inf_trans[13])/population;
                            n32 += (r_trans[9] - s_inf_trans[14] - s_inf_trans[15])/population;
                            n34 += (r_trans[10] - s_inf_trans[16] - s_inf_trans[17])/population;
                            n41 += (r_trans[12] - s_inf_trans[18] - s_inf_trans[19])/population;
                            n42 += (r_trans[13] - s_inf_trans[20] - s_inf_trans[21])/population;
                            n43 += (r_trans[14] - s_inf_trans[22] - s_inf_trans[23])/population;
                            r += (s_inf_trans[0] + s_inf_trans[2] + s_inf_trans[4] + s_inf_trans[6] + s_inf_trans[8] + s_inf_trans[10] + s_inf_trans[12] + s_inf_trans[14] + s_inf_trans[16] + s_inf_trans[18] + s_inf_trans[20] + s_inf_trans[22] - rec_trans[0])/population;
                            
                            ')

dengue_rmeasure <- Csnippet('
                        // measurement model of y1 given H = (n1 + n21 + n31 + n41)*population, for instance
                        // mean: kappa*H, variance: kappa*H + (kappa^2)(H^2)/psi
                        int population = 2217968;
                        y1 = rnbinom_mu(psi, kappa*((n1 + n21 + n31 + n41)*population));
                        y2 = rnbinom_mu(psi, kappa*((n2 + n12 + n32 + n42)*population));
                        y3 = rnbinom_mu(psi, kappa*((n3 + n13 + n23 + n43)*population));
                        y4 = rnbinom_mu(psi, kappa*((n4 + n14 + n24 + n34)*population));
                        ')

dengue_dmeasure <- Csnippet('
                        // measurement density of y1 given H = (n1 + n21 + n31 + n41)*population, for instance
                        // mean: kappa*H, variance: kappa*H + (kappa^2)(H^2)/psi
                        int population = 2217968;
                        double w1, w2, w3, w4;
                        w1 = dnbinom_mu(y1, psi, kappa*((n1 + n21 + n31 + n41)*population), give_log);
                        w2 = dnbinom_mu(y2, psi, kappa*((n2 + n12 + n32 + n42)*population), give_log);
                        w3 = dnbinom_mu(y3, psi, kappa*((n3 + n13 + n23 + n43)*population), give_log);
                        w4 = dnbinom_mu(y4, psi, kappa*((n4 + n14 + n24 + n34)*population), give_log);
                        lik = (give_log) ? (w1 + w2 + w3 + w4) : (w1*w2*w3*w4);
                        ')

dengue_statenames <- c("s","n1", "n2", "n3", "n4",
                       "r1", "r2", "r3", "r4",
                       "n12", "n13", "n14",
                       "n21", "n23", "n24",
                       "n31", "n32", "n34",
                       "n41", "n42", "n43",
                       "r")

dengue_paramnames <- c("phi", "Gamma", "kappa", "psi", "mu", "sigma", "bet")
dengue_obsnames <- c("y1", "y2", "y3", "y4")
dengue_fromEstimationScale <- "
Tphi = exp(phi) + 1;
TGamma = exp(Gamma) + 1;
Tkappa = expit(kappa);
Tpsi = exp(psi);
"

dengue_toEstimationScale <- "
Tphi = log(phi - 1);
TGamma = log(Gamma - 1);
Tkappa = logit(kappa);
Tpsi = log(psi);
"

dengue_initializer <- "
s = (0.1);
n1 = (0.002);
n2 = (0.004);
n3 = (0.003);
n4 = (0.001);
r1 = 0.196;
r2 = 0.199;
r3 = 0.199;
r4 = 0.197;
n12 = 0.01;
n13 = 0.01;
n14 = 0.01;
n21 = 0.01;
n23 = 0.01;
n24 = 0.01;
n31 = 0.01;
n32 = 0.01;
n34 = 0.01;
n41 = 0.01;
n42 = 0.01;
n43 = 0.01;
r = 0;
"

dd_test_large = data.frame(week = c(0:1000000), y1 = c(0:1000000), y2 = c(0:1000000), y3 = c(0:1000000), y4 = c(0:1000000))
subset_dd = dd[100:308,]
dengue_pomp_test <- pomp(data = dd_test_large,
                         times = "week",
                         skeleton = map(dengue_skel, 1/7),
                         t0 = 0,
                         fromEstimationScale=Csnippet(dengue_fromEstimationScale),
                         toEstimationScale=Csnippet(dengue_toEstimationScale),
                         obsnames = dengue_obsnames,
                         statenames = dengue_statenames,
                         paramnames = dengue_paramnames,
                         initializer = Csnippet(dengue_initializer),
                         rprocess = euler.sim(
                           step.fun = dengue_rprocess,
                           delta.t=1/7),
                         rmeasure = dengue_rmeasure,
                         dmeasure = dengue_dmeasure
)
dengue_pomp_only_skel <- pomp(data = dd_test_large,
                              times = "week",
                              skeleton = map(dengue_skel, 1/7),
                              paramnames = c("mu","sigma", "bet", 
                                             "phi","Gamma"),
                              initializer = Csnippet(dengue_initializer),
                              statenames = c("s",
                                             "n1", "n2", "n3", "n4",
                                             "r1", "r2", "r3", "r4",
                                             "n12", "n13", "n14",
                                             "n21", "n23", "n24",
                                             "n31", "n32", "n34",
                                             "n41", "n42", "n43",
                                             "r"),
                              t0 = 0
)
dengue_pomp <- pomp(
  data = subset_dd,
  times = "week",
  skeleton = map(dengue_skel, 1/7),
  t0 = 0,
  fromEstimationScale=Csnippet(dengue_fromEstimationScale),
  toEstimationScale=Csnippet(dengue_toEstimationScale),
  obsnames = dengue_obsnames,
  statenames = dengue_statenames,
  paramnames = dengue_paramnames,
  initializer = Csnippet(dengue_initializer),
  rprocess = euler.sim(
    step.fun = dengue_rprocess,
    delta.t=1/7),
  rmeasure = dengue_rmeasure,
  dmeasure = dengue_dmeasure
)

population = 2217968
t = trajectory(dengue_pomp_test, params = c(phi = 2.1, Gamma = 1.9, kappa = 0.5, psi = 1, mu = 1/(78.29*365), sigma = 1/3.65, bet = 400/365))
t2 = as.data.frame(t(as.data.frame(t[,1,]))) %>% mutate(week = dd_test_large$week)

dengue_initializer <- function(params, t0, ...){
  return(c(
    s = t2[1000001,'s'],
    n1 = t2[1000001,'n1'],
    n2 = t2[1000001,'n2'],
    n3 = t2[1000001,'n3'],
    n4 = t2[1000001,'n4'],
    r1 = t2[1000001,'r1'],
    r2 = t2[1000001,'r2'],
    r3 = t2[1000001,'r3'],
    r4 = t2[1000001,'r4'],
    n12 = t2[1000001,'n12'],
    n13 = t2[1000001,'n13'],
    n14 = t2[1000001,'n14'],
    n21 = t2[1000001,'n21'],
    n23 = t2[1000001,'n23'],
    n24 = t2[1000001,'n24'],
    n31 = t2[1000001,'n31'],
    n32 = t2[1000001,'n32'],
    n34 = t2[1000001,'n34'],
    n41 = t2[1000001,'n41'],
    n42 = t2[1000001,'n42'],
    n43 = t2[1000001,'n43'],
    r = t2[1000001,'r']))
}
dengue_pomp <- pomp(dengue_pomp, initializer = dengue_initializer)

# part to be run in flux

run_level <- 3
switch(run_level,
       {dengue_Np=100; dengue_Nmif=10; dengue_Neval=10; dengue_Nglobal=10; dengue_Nlocal=10}, 
       {dengue_Np=15000; dengue_Nmif=65; dengue_Neval=10; dengue_Nglobal=10; dengue_Nlocal=10}, 
       {dengue_Np=50000; dengue_Nmif=75; dengue_Neval=10; dengue_Nglobal=25; dengue_Nlocal=10}
)

dengue_fixed_params <- c(mu = 1/(78.29*365), sigma = 1/3.65, bet = 400/365)

require(doParallel)
cores <- 23
registerDoParallel(cores)
mcopts <- list(set.seed=TRUE)

set.seed(396658101,kind="L'Ecuyer")

dengue_box <- rbind(
  kappa=c(0.05,0.7),
  phi=c(2, 6),
  Gamma = c(2, 6),
  psi = c(1,20)
)

dengue_rw.sd <- 0.02
dengue_cooling.fraction.50 <- 0.5
stew(file=sprintf("box_eval_part5-%d.rda",run_level),{
  
  t_global <- system.time({
    mifs_global <- foreach(i=1:dengue_Nglobal,.packages='pomp', .combine=c, .options.multicore=mcopts) %dopar%  mif2(
      dengue_pomp,
      start=c(apply(dengue_box,1,function(x)runif(1,x[1],x[2])),dengue_fixed_params),
      Np = dengue_Np,
      Nmif = dengue_Nmif,
      cooling.type = "geometric",
      cooling.fraction.50 = dengue_cooling.fraction.50,
      transform = TRUE,
      rw.sd=rw.sd(
        phi = dengue_rw.sd,
        Gamma = dengue_rw.sd,
        kappa = dengue_rw.sd,
        psi = dengue_rw.sd
      )
    )
  })
},seed=1270401374,kind="L'Ecuyer")

