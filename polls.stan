data {
          int N;
          int T;
          vector[N] y;
          vector[N] s;
          int time[N];
          int H;
          int house[N];
          // initial and final values
          real xi_init;
          real xi_final;
          real delta_loc;
          real zeta_scale;
          real tau_scale;
        }
        parameters {
          vector[T - 1] omega;
          real tau;
          vector[H] delta_raw;
          real zeta;
        }
        transformed parameters {
          vector[H] delta;
          vector[T - 1] xi;
          vector[N] mu;
          // this is necessary. If not centered the model is unidentified
          delta = (delta_raw - mean(delta_raw)) / sd(delta_raw) * zeta;
          xi[1] = xi_init;
          for (i in 2:(T - 1)) {
            xi[i] = xi[i - 1] + tau * omega[i - 1];
          }
          for (i in 1:N) {
            mu[i] = xi[time[i]] + delta[house[i]];
          }
        }
        model {
          // house effects
          delta_raw ~ normal(0., 1.);
          zeta ~ normal(0., zeta_scale);
          // latent state innovations
          omega ~ normal(0., 1.);
          // scale of innovations
          tau ~ cauchy(0, tau_scale);
          // final known effect
          xi_final ~ normal(xi[T - 1], 5);
          // daily polls
          y ~ normal(mu, s);
        }
