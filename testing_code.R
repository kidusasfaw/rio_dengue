dengue_skel <- Csnippet('
                         int numstrains = 4;
                        double phi[4][4] = {{0, phi12, phi13, phi14}, {phi21, 0, phi23, phi24}, {phi31, phi32, 0, phi34}, {phi41, phi42, phi43, 0}};
                        double n_sec[4][4] = {{0, n12, n13, n14}, {n21, 0, n23, n24}, {n31, n32, 0, n34}, {n41, n42, n43, 0}};
                        double gammas[4][4] = {{0, gamma12, gamma13, gamma14}, {gamma21, 0, gamma23, gamma24}, {gamma31, gamma32, 0, gamma34}, {gamma41, gamma42, gamma43, 0}};
                        double lambdas[4] = {0, 0, 0, 0};
                        double n[4] = {n1, n2, n3, n4};
                        double rs[4] = {r1, r2, r3, r4};
                        double deltas[10];
                        double deltas_sec[4][4];
                        double sum_lambdas = 0;
                        double sumprod = 0;
                        double sumprod2 = 0;
                        /*printf("%f", s);*/
                        /*printf("%f", n1);*/
                        
                        for(int i = 0; i < numstrains; i++){
                        sumprod = 0;
                        sumprod2 = 0;
                        for(int j = 0; j < numstrains; j++){
                        if(i == j){
                        sumprod += 0;
                        }
                        else{
                        sumprod += phi[j][i]*n_sec[j][i];
                        }
                        }
                        lambdas[i] = bet*(n[i] + sumprod);
                        for(int j = 0; j < numstrains; j++){
                        if(i == j){
                        sumprod2 += 0;
                        }
                        else{
                        sumprod2 += gammas[i][j]*lambdas[j];
                        }
                        }
                        sum_lambdas += lambdas[i];
                        deltas[i+5] = sigma*n[i] - rs[i]*(mu + sumprod2);
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
                        double boop =  deltas[0] + deltas[1] + deltas[2] + deltas[3] + deltas[4] + deltas[5] + deltas[6] + deltas[7] + deltas[8] + deltas[9] + deltas_sec[0][1] + deltas_sec[0][2] + deltas_sec[0][3] + deltas_sec[1][0] + deltas_sec[1][2] + deltas_sec[1][3] + deltas_sec[2][0] + deltas_sec[2][1] + deltas_sec[2][3] + deltas_sec[3][0] + deltas_sec[3][1] + deltas_sec[3][2];
                        if(boop > 0.01119){
                        /*printf("%f", deltas[0]);*/
                        printf("%f", s);
                        printf("s");
                        /*printf("%.10f", deltas[1]);*/
                        
                        /*printf("s");*/
                        /*printf("%.10f", deltas[2]);*/
                        
                        /*printf("s");*/
                        /*printf("%.10f", deltas[3]);*/
                        
                        /*printf("s");*/
                        /*printf("%.10f", deltas[4]);*/
                        
                        /*printf("s");*/
                        printf("%.10f", sum_lambdas);
                        printf("s");
                        printf("%.10f", lambdas[0]);
                        printf("s");
                        printf("%.10f", n[0]);
                        printf("s");
                        printf("%.10f", lambdas[1]);
                        printf("s");
                        printf("%.10f", n[1]);
                        printf("s");
                        printf("%.10f", lambdas[2]);
                        printf("s");
                        printf("%.10f", n[2]);
                        printf("s");
                        printf("%.10f", lambdas[3]);
                        printf("s");
                        printf("%.10f", n[3]);
                        printf("s");
                        /*printf("%.10f", deltas[5]);*/
                        printf("%.10f", rs[0]);
                        printf("s");
                        /*printf("%.10f", deltas[6]);*/
                        printf("%.10f", rs[1]);
                        printf("s");
                        /*printf("%.10f", deltas[7]);*/
                        printf("%.10f", rs[2]);
                        printf("s");
                        /*printf("%.10f", deltas[8]);*/
                        printf("%.10f", rs[3]);
                        printf("s");
                        /*printf("%.10f", deltas[9]);*/
                        printf("%.10f", r);
                        printf("s");
                        /*printf("%.10f", deltas_sec[0][1]);*/
                        printf("%.10f", n_sec[0][1]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[0][2]);*/
                        printf("%.10f", n_sec[0][2]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[0][3]);*/
                        printf("%.10f", n_sec[0][3]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[1][0]);*/
                        printf("%.10f", deltas_sec[1][0]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[1][2]);*/
                        printf("%.10f", deltas_sec[1][2]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[1][3]);*/
                        printf("%.10f", deltas_sec[1][3]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[2][0]);*/
                        printf("%.10f", deltas_sec[2][0]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[2][1]);*/
                        printf("%.10f", deltas_sec[2][1]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[2][3]);*/
                        printf("%.10f", deltas_sec[2][3]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[3][0]);*/
                        printf("%.10f", deltas_sec[3][0]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[3][1]);*/
                        printf("%.10f", deltas_sec[3][1]);
                        printf("s");
                        /*printf("%.10f", deltas_sec[3][2]);*/
                        printf("%.10f", deltas_sec[3][2]);
                        printf("end iteration");
                        }
                        ')

dengue <- pomp(data = as.data.frame(dd),
               times = "week",
               skeleton = map(dengue_skel, 1),
               paramnames = c("mu","sigma", "bet", 
                              "phi12", "phi13", "phi14",
                              "phi21", "phi23", "phi24",
                              "phi31", "phi32", "phi34",
                              "phi41", "phi42", "phi43",
                              "gamma12", "gamma13", "gamma14",
                              "gamma21", "gamma23", "gamma24",
                              "gamma31", "gamma32", "gamma34",
                              "gamma41", "gamma42", "gamma43",
                              "s.0",
                              "n1.0", "n2.0", "n3.0", "n4.0",
                              "r1.0", "r2.0", "r3.0", "r4.0",
                              "n12.0", "n13.0", "n14.0",
                              "n21.0", "n23.0", "n24.0",
                              "n31.0", "n32.0", "n34.0",
                              "n41.0", "n42.0", "n43.0",
                              "r.0"),
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

traj = trajectory(dengue, params = c(mu = 0, sigma = 7/3.65, bet = 100/52, 
                                     phi12 = 1.1, phi13 = 1.1, phi14 = 1.1,
                                     phi21 = 1.1, phi23 = 1.1, phi24 = 1.1,
                                     phi31 = 1.1, phi32 = 1.1, phi34 = 1.1,
                                     phi41 = 1.1, phi42 = 1.1, phi43 = 1.1,
                                     gamma12 = 1.5, gamma13 = 1.5, gamma14 = 1.5,
                                     gamma21 = 1.5, gamma23 = 1.5, gamma24 = 1.5,
                                     gamma31 = 1.5, gamma32 = 1.5, gamma34 = 1.5,
                                     gamma41 = 1.5, gamma42 = 1.5, gamma43 = 1.5,
                                     s.0 = (2217968-62)/2217968,
                                     n1.0 = (30/2217968), n2.0 = (10/2217968), n3.0 = (15/2217968), n4.0 = (7/2217968),
                                     r1.0 = 0, r2.0 = 0, r3.0 = 0, r4.0 = 0,
                                     n12.0 = 0, n13.0 = 0, n14.0 = 0,
                                     n21.0 = 0, n23.0 = 0, n24.0 = 0,
                                     n31.0 = 0, n32.0 = 0, n34.0 = 0,
                                     n41.0 = 0, n42.0 = 0, n43.0 = 0,
                                     r.0 = 0))
traj2 = as.data.frame(t(as.data.frame(traj[,1,]))) %>% mutate(week = dd$week)
class(traj2)
ggplot(data=traj2) + 
  geom_line(mapping = aes(x = week, y = 2217968*(n1 + n21 + n31 + n41)), color = 'blue', alpha = 0.25) + 
  geom_line(mapping = aes(x = week, y = 2217968*(n2 + n12 + n32 + n42)), color = 'red', alpha = 0.25) + 
  geom_line(mapping = aes(x = week, y = 2217968*(n3 + n13 + n23 + n43)), color = 'green', alpha = 0.25) + 
  geom_line(mapping = aes(x = week, y = 2217968*(n4 + n14 + n24 + n34)), color = 'yellow', alpha = 0.25) + 
  geom_line(data = sj_dengue, mapping = aes(x = week, y = denv1_cases))













dengue_skel2 <- Csnippet('
                         int numstrains = 4;
                         double phi[4][4] = {{0, phi12, phi13, phi14}, {phi21, 0, phi23, phi24}, {phi31, phi32, 0, phi34}, {phi41, phi42, phi43, 0}};
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
                         /*printf("%f", s);*/
                         /*printf("%f", n1);*/
                         
                         for(int i = 0; i < numstrains; i++){
                         sumprod = 0;
                         for(int j = 0; j < numstrains; j++){
                         if(i == j){
                         sumprod += 0;
                         }
                         else{
                         sumprod += phi[j][i]*n_sec[j][i];
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
                         Ds = deltas[0];
                         Dn1 = deltas[1];
                         Dn2 = deltas[2];
                         Dn3 = deltas[3];
                         Dn4 = deltas[4];
                         Dr1 = deltas[5];
                         Dr2 = deltas[6];
                         Dr3 = deltas[7];
                         Dr4 = deltas[8];
                         Dr = deltas[9];
                         Dn12 = deltas_sec[0][1];
                         Dn13 = deltas_sec[0][2];
                         Dn14 = deltas_sec[0][3];
                         Dn21 = deltas_sec[1][0];
                         Dn23 = deltas_sec[1][2];
                         Dn24 = deltas_sec[1][3];
                         Dn31 = deltas_sec[2][0];
                         Dn32 = deltas_sec[2][1];
                         Dn34 = deltas_sec[2][3];
                         Dn41 = deltas_sec[3][0];
                         Dn42 = deltas_sec[3][1];
                         Dn43 = deltas_sec[3][2];
                         double boop =  deltas[0] + deltas[1] + deltas[2] + deltas[3] + deltas[4] + deltas[5] + deltas[6] + deltas[7] + deltas[8] + deltas[9] + deltas_sec[0][1] + deltas_sec[0][2] + deltas_sec[0][3] + deltas_sec[1][0] + deltas_sec[1][2] + deltas_sec[1][3] + deltas_sec[2][0] + deltas_sec[2][1] + deltas_sec[2][3] + deltas_sec[3][0] + deltas_sec[3][1] + deltas_sec[3][2];
                         if(boop > 0.00001){                        
                         printf("%f", deltas[0]);
                         /*printf("%f", s);*/
                         printf("s");
                         printf("%.10f", deltas[1]);
                         
                         printf("s");
                         printf("%.10f", deltas[2]);
                         
                         printf("s");
                         printf("%.10f", deltas[3]);
                         
                         printf("s");
                         printf("%.10f", deltas[4]);
                         
                         printf("s");
                         /*printf("%.10f", sum_lambdas);*/
                         /*printf("s");*/
                         /*printf("%.10f", lambdas[0]);*/
                         /*printf("s");*/
                         /*printf("%.10f", n[0]);*/
                         /*printf("s");*/
                         /*printf("%.10f", lambdas[1]);*/
                         /*printf("s");*/
                         /*printf("%.10f", n[1]);*/
                         /*printf("s");*/
                         /*printf("%.10f", lambdas[2]);*/
                         /*printf("s");*/
                         /*printf("%.10f", n[2]);*/
                         /*printf("s");*/
                         /*printf("%.10f", lambdas[3]);*/
                         /*printf("s");*/
                         /*printf("%.10f", n[3]);*/
                         /*printf("s");*/
                         printf("%.10f", deltas[5]);
                         /*printf("%.10f", rs[0]);*/
                         printf("ss");
                         printf("%.10f", deltas[6]);
                         /*printf("%.10f", rs[1]);*/
                         printf("s");
                         printf("%.10f", deltas[7]);
                         /*printf("%.10f", rs[2]);*/
                         printf("s");
                         printf("%.10f", deltas[8]);
                         /*printf("%.10f", rs[3]);*/
                         printf("s");
                         printf("%.10f", deltas[9]);
                         /*printf("%.10f", r);*/
                         printf("s");
                         printf("%.10f", deltas_sec[0][1]);
                         /*printf("%.10f", n_sec[0][1]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[0][2]);
                         /*printf("%.10f", n_sec[0][2]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[0][3]);
                         /*printf("%.10f", n_sec[0][3]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[1][0]);
                         /*printf("%.10f", n_sec[1][0]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[1][2]);
                         /*printf("%.10f", n_sec[1][2]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[1][3]);
                         /*printf("%.10f", n_sec[1][3]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[2][0]);
                         /*printf("%.10f", n_sec[2][0]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[2][1]);
                         /*printf("%.10f", n_sec[2][1]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[2][3]);
                         /*printf("%.10f", n_sec[2][3]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[3][0]);
                         /*printf("%.10f", n_sec[3][0]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[3][1]);
                         /*printf("%.10f", n_sec[3][1]);*/
                         printf("s");
                         printf("%.10f", deltas_sec[3][2]);
                         /*printf("%.10f", n_sec[3][2]);*/
                         printf("%f", sigma*n[0] - rs[0]*(mu + gamma12*lambdas[1] + gamma13*lambdas[2] + gamma14*lambdas[3]));
                         printf("end iteration");
                         }
                         /*printf("%.10f", sum_lambdas);*/
                         ')
df = data.frame(week = c(0:1000000), y1 = c(0:1000000), y2 = c(0:1000000), y3 = c(0:1000000), y4 = c(0:1000000))
dengue2 <- pomp(data = df,
                times = "week",
                skeleton = vectorfield(dengue_skel2),
                paramnames = c("mu","sigma", "bet", 
                               "phi12", "phi13", "phi14",
                               "phi21", "phi23", "phi24",
                               "phi31", "phi32", "phi34",
                               "phi41", "phi42", "phi43",
                               "gamma12", "gamma13", "gamma14",
                               "gamma21", "gamma23", "gamma24",
                               "gamma31", "gamma32", "gamma34",
                               "gamma41", "gamma42", "gamma43",
                               "s.0",
                               "n1.0", "n2.0", "n3.0", "n4.0",
                               "r1.0", "r2.0", "r3.0", "r4.0",
                               "n12.0", "n13.0", "n14.0",
                               "n21.0", "n23.0", "n24.0",
                               "n31.0", "n32.0", "n34.0",
                               "n41.0", "n42.0", "n43.0",
                               "r.0"),
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
traj = trajectory(dengue2, params = c(mu = 1/(70*365), sigma = 1/3.65, bet = 400/365, 
                                      phi12 = 1.2, phi13 = 1.2, phi14 = 1.2,
                                      phi21 = 1.2, phi23 = 1.2, phi24 = 1.2,
                                      phi31 = 1.2, phi32 = 1.2, phi34 = 1.2,
                                      phi41 = 1.2, phi42 = 1.2, phi43 = 1.2,
                                      gamma12 = 1.5, gamma13 = 1.5, gamma14 = 1.5,
                                      gamma21 = 1.5, gamma23 = 1.5, gamma24 = 1.5,
                                      gamma31 = 1.5, gamma32 = 1.5, gamma34 = 1.5,
                                      gamma41 = 1.5, gamma42 = 1.5, gamma43 = 1.5,
                                      s.0 = (0.1),
                                      n1.0 = (0.001), n2.0 = (0.001), n3.0 = (0.001), n4.0 = (0.001),
                                      r1.0 = 0.199, r2.0 = 0.199, r3.0 = 0.199, r4.0 = 0.199,
                                      n12.0 = 0.01, n13.0 = 0.01, n14.0 = 0.01,
                                      n21.0 = 0.01, n23.0 = 0.01, n24.0 = 0.01,
                                      n31.0 = 0.01, n32.0 = 0.01, n34.0 = 0.01,
                                      n41.0 = 0.01, n42.0 = 0.01, n43.0 = 0.01,
                                      r.0 = 0))

traj2 = as.data.frame(t(as.data.frame(traj[,1,]))) %>% mutate(week = df$week)
class(traj2)
ggplot(data=(traj2%>% filter(week > 300000 & week < 400000))) + 
  geom_line(mapping = aes(x = week, y = r1), color = 'blue', alpha = 0.25) 
ggplot(data=traj2)  
geom_line(mapping = aes(x = week, y = 2217968*(s)))

#geom_line(data = sj_dengue, mapping = aes(x = week, y = denv1_cases))

traj2$s[988] + traj2$r[988] + traj2$n1[988] + traj2$n2[988] + traj2$n3[988] + traj2$n4[988] + traj2$n12[988] + traj2$n13[988] +traj2$n14[988] +traj2$n21[988] +traj2$n23[988] +traj2$n24[988] +traj2$n31[988] +traj2$n32[988] +traj2$n34[988] +traj2$n41[988] +traj2$n42[988] +traj2$n43[988] +traj2$r1[988] + traj2$r2[988] + traj2$r3[988] + traj2$r4[988]












# Ds = ((deltas[0]) < (-s) ? (0) : (s + deltas[0]));
# Dn1 = ((deltas[1]) < (-n1) ? (0) : (n1 + deltas[1]));
# Dn2 = ((deltas[2]) < (-n2) ? (0) : (n2 + deltas[2]));
# Dn3 = ((deltas[3]) < (-n3) ? (0) : (n3 + deltas[3]));
# Dn4 = ((deltas[4]) < (-n4) ? (0) : (n4 + deltas[4]));
# Dr1 = ((deltas[5]) < (-r1) ? (0) : (r1 + deltas[5]));
# Dr2 = ((deltas[6]) < (-r2) ? (0) : (r2 + deltas[6]));
# Dr3 = ((deltas[7]) < (-r3) ? (0) : (r3 + deltas[7]));
# Dr4 = ((deltas[8]) < (-r4) ? (0) : (r4 + deltas[8]));
# Dr = ((deltas[9]) < (-r) ? (0) : (r + deltas[9]));
# Dn12 = ((deltas_sec[0][1]) < (-n12) ? (0) : (n12 + deltas_sec[0][1]));
# Dn13 = ((deltas_sec[0][2]) < (-n13) ? (0) : (n13 + deltas_sec[0][2]));
# Dn14 = ((deltas_sec[0][3]) < (-n14) ? (0) : (n14 + deltas_sec[0][3]));
# Dn21 = ((deltas_sec[1][0]) < (-n21) ? (0) : (n21 + deltas_sec[1][0]));
# Dn23 = ((deltas_sec[1][2]) < (-n23) ? (0) : (n23 + deltas_sec[1][2]));
# Dn24 = ((deltas_sec[1][3]) < (-n24) ? (0) : (n24 + deltas_sec[1][3]));
# Dn31 = ((deltas_sec[2][0]) < (-n31) ? (0) : (n31 + deltas_sec[2][0]));
# Dn32 = ((deltas_sec[2][1]) < (-n32) ? (0) : (n32 + deltas_sec[2][1]));
# Dn34 = ((deltas_sec[2][3]) < (-n34) ? (0) : (n34 + deltas_sec[2][3]));
# Dn41 = ((deltas_sec[3][0]) < (-n41) ? (0) : (n41 + deltas_sec[3][0]));
# Dn42 = ((deltas_sec[3][1]) < (-n42) ? (0) : (n42 + deltas_sec[3][1]));
# Dn43 = ((deltas_sec[3][2]) < (-n43) ? (0) : (n43 + deltas_sec[3][2]));
# */
#   
#   double boop =  deltas[0] + deltas[1] + deltas[2] + deltas[3] + deltas[4] + deltas[5] + deltas[6] + deltas[7] + deltas[8] + deltas[9] + deltas_sec[0][1] + deltas_sec[0][2] + deltas_sec[0][3] + deltas_sec[1][0] + deltas_sec[1][2] + deltas_sec[1][3] + deltas_sec[2][0] + deltas_sec[2][1] + deltas_sec[2][3] + deltas_sec[3][0] + deltas_sec[3][1] + deltas_sec[3][2];
# if(boop > 0.00001){                        
#   printf("%f", deltas[0]);
#   /*printf("%f", s);*/
#     printf("s");
#   printf("%.10f", deltas[1]);
#   
#   printf("s");
#   printf("%.10f", deltas[2]);
#   
#   printf("s");
#   printf("%.10f", deltas[3]);
#   
#   printf("s");
#   printf("%.10f", deltas[4]);
#   
#   printf("s");
#   /*printf("%.10f", sum_lambdas);*/
#     /*printf("s");*/
#     /*printf("%.10f", lambdas[0]);*/
#     /*printf("s");*/
#     /*printf("%.10f", n[0]);*/
#     /*printf("s");*/
#     /*printf("%.10f", lambdas[1]);*/
#     /*printf("s");*/
#     /*printf("%.10f", n[1]);*/
#     /*printf("s");*/
#     /*printf("%.10f", lambdas[2]);*/
#     /*printf("s");*/
#     /*printf("%.10f", n[2]);*/
#     /*printf("s");*/
#     /*printf("%.10f", lambdas[3]);*/
#     /*printf("s");*/
#     /*printf("%.10f", n[3]);*/
#     /*printf("s");*/
#     printf("%.10f", deltas[5]);
#   /*printf("%.10f", rs[0]);*/
#     printf("ss");
#   printf("%.10f", deltas[6]);
#   /*printf("%.10f", rs[1]);*/
#     printf("s");
#   printf("%.10f", deltas[7]);
#   /*printf("%.10f", rs[2]);*/
#     printf("s");
#   printf("%.10f", deltas[8]);
#   /*printf("%.10f", rs[3]);*/
#     printf("s");
#   printf("%.10f", deltas[9]);
#   /*printf("%.10f", r);*/
#     printf("s");
#   printf("%.10f", deltas_sec[0][1]);
#   /*printf("%.10f", n_sec[0][1]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[0][2]);
#   /*printf("%.10f", n_sec[0][2]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[0][3]);
#   /*printf("%.10f", n_sec[0][3]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[1][0]);
#   /*printf("%.10f", n_sec[1][0]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[1][2]);
#   /*printf("%.10f", n_sec[1][2]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[1][3]);
#   /*printf("%.10f", n_sec[1][3]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[2][0]);
#   /*printf("%.10f", n_sec[2][0]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[2][1]);
#   /*printf("%.10f", n_sec[2][1]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[2][3]);
#   /*printf("%.10f", n_sec[2][3]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[3][0]);
#   /*printf("%.10f", n_sec[3][0]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[3][1]);
#   /*printf("%.10f", n_sec[3][1]);*/
#     printf("s");
#   printf("%.10f", deltas_sec[3][2]);
#   /*printf("%.10f", n_sec[3][2]);*/
#     printf("%f", sigma*n[0] - rs[0]*(mu + gamma12*lambdas[1] + gamma13*lambdas[2] + gamma14*lambdas[3]));
#   printf("end iteration");