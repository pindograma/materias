#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame fillEmptyDays(DataFrame cand_days_df, Date first_polling_date) {
  DateVector date = cand_days_df["date"];
  NumericVector day_average = cand_days_df["day_average"];
  NumericVector day_weight = cand_days_df["day_weight"];
  NumericVector go_back = cand_days_df["go_back"];
  NumericVector imputed = cand_days_df["imputed"];
  
  for (unsigned int i = 0; i < cand_days_df.nrows(); i++) {
    if (i == 0 || !NumericVector::is_na(day_average[i]) || (Date)date[i] <= first_polling_date) {
      continue;
    }
    
    if (imputed[i - 1]) {
      day_average[i] = day_average[i - 1];
      day_weight[i] = day_weight[i - 1];
      continue;
    }
    
    double new_average = 0;
    double sum_weights = 0;
    
    for (unsigned int j = go_back[i] - 1; j < i; j++) {
      new_average += day_average[j] * day_weight[j];
      sum_weights += day_weight[j];
    }
    
    new_average /= sum_weights;
    day_average[i] = new_average;
    
    day_weight[i] = (sum_weights / (i - (go_back[i] - 1)));
  }
  
  cand_days_df["day_average"] = day_average;
  cand_days_df["day_weight"] = day_weight;
  
  return cand_days_df;
}

// [[Rcpp::export]]
NumericVector calculateFinalAverage(NumericVector day_average,
                                    NumericVector day_weight,
                                    LogicalVector imputed,
                                    int n,
                                    bool do_weighting,
                                    double temporal_reduction_factor,
                                    double squo_reduction_factor) {
  std::vector<double> ret;
  
  unsigned int first_poll_idx = 0;
  while (1) {
    if (!NumericVector::is_na(day_average[first_poll_idx])) break;
    else first_poll_idx++;
  }
  
  for (unsigned int i = 0; i < day_average.size(); i++) {
    if (i < first_poll_idx) {
      ret.push_back(NA_REAL);
      continue;
    }
    
    if (imputed[i]) {
      ret.push_back(ret.back());
      continue;
    }
    
    unsigned int distance_from_end = (day_average.size() - i);
    double counter_start = day_average.size() - 4;
    
    double coef = i <= counter_start ? 0 : 0.05 * (day_average.size() - counter_start - distance_from_end);
    
    int go_back = first_poll_idx;
    if (i - first_poll_idx >= n) {
      if (distance_from_end < n) {
        go_back = i - distance_from_end;
      } else {
        go_back = i - (n - 1);
      }
    }
    
    double average = 0;
    double sum_weights = 0;
    
    for (unsigned int j = go_back; j <= i; j++) {
      unsigned int previous_imputed_values = 0;
      for (int k = j; k >= 0; k--) {
        if (!imputed[k]) break;
        previous_imputed_values++;
      }
      
      double new_weight = day_weight[j];
      if (do_weighting) {
        new_weight *= pow(temporal_reduction_factor - coef, i - j);
        
        if (imputed[j]) {
          new_weight *= pow(squo_reduction_factor - coef, previous_imputed_values);
        }
      }
      
      average += day_average[j] * new_weight;
      sum_weights += new_weight;
    }
    
    average /= sum_weights;
    ret.push_back(average);
  }
  
  return wrap(ret);
}