
// This is a wrapper around pow() that checks if the input x is below zero. If 
// it is then it returns zero. We use it as floating point errors for extinct
// species can result in abundances below zero, hence a NaN in other species 
// when interactions are computed.
double pow2(double x, 
            double y) { 
  
  double result=0.0;
  
  if (x >= 0.0) { 
    result = pow(x,y);
  }
  
  return result;
}
