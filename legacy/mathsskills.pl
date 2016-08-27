/* -------------------------------------------------------------------
   Mathskills for SWI-Prolog version 7.2.3.
   
   Interdependency map of skills for basic mathematics (numeracy and algebra).
   
   V1.0 2016-5-27.  Chris Sangwin
   -------------------------------------------------------------------
   Usage:

   Tag atoms are used for each skill.  These tags should be evocative of the skill itself.
   
   Obvious primary subskills can be reflected in the tags
   
    num_add_neg_neg ;
    num_mul_pos_pos ;
    num_mul_neg_pos ;
    num_mul_neg_neg ;
    num_dec ;
    num_rat ;

   The "atom_prefix" function can be used to identify possible subskills based on the tag.   
   mskill(X,_), atom_prefix(X,num).

   ------------------------------------------------------------------ */

/* mskill is the basic mathematical skill.  
   mskill(Tag, Description)                 
*/
:- discontiguous(mskill/2).

/* exq is an example quesion for that skill. 
   exq(Tag, Question)                        
*/
:- discontiguous(exq/2).

/* skill_dep indicates where a Lower level skill is required 
   by a Higher level skill
   skill_dep(Lower, Higher)                        
*/
:- discontiguous(skill_dep/2).

/* diagnosis References that skill in the Diagnosis system. */
/* See Applyby 1997.                                        */
/* diagnosis(Tag, DisgnosisTag)                             */
:- discontiguous(diagnosis/2).

/* Dealing with numbers.                                    */

diagnosis(num_mul_neg_neg, 101).
mskill(num_mul_neg_neg, "Multiplication of two negative numbers").
exq(num_mul_neg_neg, "Calculate \\( -3\\times -5\\).").

diagnosis(num_mul_neg_pos, 102).
mskill(num_mul_neg_pos, "Multiplication of positive and negative numbers").
skill_dep(num_mul_neg_pos, num_mul_neg_neg).
exq(num_mul_neg_pos, "Calculate \\( 3\\times -5\\).").
exq(num_mul_neg_pos, "Calculate \\( -3\\times 5\\).").

diagnosis(num_add_neg_neg, 103).
mskill(num_add_neg_neg, "Addition of two negative numbers").
exq(num_add_neg_neg, "Calculate \\( -3 + -5\\).").

mskill(num_mul_pos_pos, "Multiplication of positive numbers").
exq(num_mul_pos_pos, "Calculate \\( 3\\times 5\\).").

diagnosis(num_dec, 104).
mskill(num_dec, "Size of decimals").
exq(num_dec, "Which is the largest of the following (a) \\(1/100\\) or(b) \\(0.00099963\\)?").

diagnosis(num_rat, 107).
mskill(num_rat, "Ratios").
exq(num_rat, "Which of the following ratios is not equal to the others?\n(a) 10 : 15 (b) 18 : 24\n(c)  2 : 3  (d)  8 : 12\n(e) -4 : -6").

diagnosis(num_fac, 108).
mskill(num_fac, "Numerical factors").

diagnosis(num_rat_inv, 109).
mskill(num_fac_inv, "If a car takes 5 hours for a journey travelling at 80  miles per hour (mph), how many hours would it take if it travelled at 25 mph?").

mskill(num_pow_pos, "+ve powers of numbers").
exq(num_pow_pos, "Calculate \\( 2^8\\).").

mskill(num_pow_neg, "-ve powers of numbers").
skill_dep(num_pow_neg, num_pow_pos).

diagnosis(ineq, 201).
mskill(ineq, "Use of <, > signs").
exq(ineq, "Which of the following inequalities are true? (a) 8-9 < 9-1$\n (b) -4 > -51\n (c) 2-5 < -13-6\n (d) -3 < 7\n (e) 19-98 > 20-03").


/*
:- begin_tests(cas).


:- end_tests(cas).
*/
