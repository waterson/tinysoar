/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* This file was automatically generated */

/* Symbolic constants */
#define SYM_ATTRIBUTE 1
#define SYM_CHOICES 2
#define SYM_CONFLICT 3
#define SYM_IMPASSE 4
#define SYM_INPUT_LINK 5
#define SYM_IO 6
#define SYM_ITEM 7
#define SYM_MULTIPLE 8
#define SYM_NIL 9
#define SYM_NONE 10
#define SYM_NO_CHANGE 11
#define SYM_OPERATOR 12
#define SYM_OUTPUT_LINK 13
#define SYM_QUIESCENCE 14
#define SYM_STATE 15
#define SYM_SUPERSTATE 16
#define SYM_TIE 17
#define SYM_TYPE 18
#define SYM_T 19
#define SYM_SENSOR_1 20
#define SYM_NAME 21
#define SYM_DRIVE 22
#define SYM_MOTOR_A 23
#define SYM_MOTOR_C 24
#define SYM_WAIT 25


#ifdef DECL_RETE_NETWORK

static struct beta_node betas[];

static struct alpha_node alphas[] = {
  { /* 0 (0x806c7a0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 24),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[35], 0 },
  { /* 1 (0x806b500) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 23),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[33], 0 },
  { /* 2 (0x806ac50) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 13),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[31], 0 },
  { /* 3 (0x806ae40) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[2], 0 },
  { /* 4 (0x8070ee0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 20),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[11], 0 },
  { /* 5 (0x806c448) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 5),
    DECLARE_NIL_SYMBOL,
    &alphas[6], &betas[9], 0 },
  { /* 6 (0x806c3b8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 6),
    DECLARE_NIL_SYMBOL,
    0, &betas[23], 0 },
  { /* 7 (0x806b420) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 25),
    &alphas[8], &betas[18], 0 },
  { /* 8 (0x806abc0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 22),
    &alphas[9], &betas[16], 0 },
  { /* 9 (0x806c348) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 16),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 9),
    0, &betas[3], 0 },
  { /* 10 (0x806cd68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    0, &betas[14], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x806c248) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 1 (0x8071008) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 2 (0x806b4b8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 3 (0x8055650) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x806c410) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 5 (0x8070ea8) */
    test_type_greater, relational_type_constant, field_value,
    { SYMBOL_TO_WORD(symbol_type_integer_constant, 512) }, 0 },
  { /* 6 (0x806c490) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 7 (0x806cdc0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 8 (0x806ce20) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 9 (0x806ce80) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 10 (0x806b338) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 11 (0x806c718) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 12 (0x806ae88) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 13 (0x8070e20) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 14 (0x806aee8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 15 (0x806ac18) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 16 (0x80556e8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 17 (0x806b4c8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 18 (0x806b558) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x8070e30) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x806bff0) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 25) } },
    { 0, { 0 } } },
  { /* 2 (0x806c258) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 3 (0x806c2d8) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 22) } },
    { 0, { 0 } } },
  { /* 4 (0x806cd10) */
    0, preference_type_better,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 4) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 3) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } } },
  { /* 5 (0x806ad90) */
    &actions[6], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_integer_constant, 100) } },
    { 0, { 0 } } },
  { /* 6 (0x806ade8) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_integer_constant, 100) } },
    { 0, { 0 } } },
  { /* 7 (0x806b398) */
    &actions[8], preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 8 (0x806b3c8) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { 0, { 0 } } },
  { /* 9 (0x806c6c0) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 10 (0x806c850) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
};

static struct production productions[] = {
  { /* 0 (0x806c7e0) */
#ifdef DEBUG
    "propose*wait",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x8066cf0) */
#ifdef DEBUG
    "propose*drive",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x806cb20) */
#ifdef DEBUG
    "prefer*drive-to-wait",
#endif
    0, &actions[4], 0, 0, support_type_isupport },
  { /* 3 (0x806aba0) */
#ifdef DEBUG
    "implement*drive",
#endif
    0, &actions[5], 0, 0, support_type_osupport },
  { /* 4 (0x806b100) */
#ifdef DEBUG
    "implement*wait",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x806c4b0) */
#ifdef DEBUG
    "drive*reconsider",
#endif
    0, &actions[9], 0, 0, support_type_osupport },
  { /* 6 (0x806ac70) */
#ifdef DEBUG
    "reconsider*wait",
#endif
    0, &actions[10], 0, 0, support_type_osupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x806c120) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x8070d80) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x806ae60) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[20], 
    &alphas[3], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x806c368) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[4], 
    &alphas[9], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x806c048) */
    beta_node_type_production,
    &betas[3], &betas[5], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 5 (0x806c390) */
    beta_node_type_memory,
    &betas[3], 0, &betas[6], 
    0, 0,
    0, 0, { 0 } },
  { /* 6 (0x806cd88) */
    beta_node_type_positive_join,
    &betas[5], &betas[7], &betas[13], 
    &alphas[10], 0,
    0, 0, { &tests[2] } },
  { /* 7 (0x806c3d8) */
    beta_node_type_positive_join,
    &betas[5], 0, &betas[8], 
    &alphas[6], 0,
    0, 0, { &tests[3] } },
  { /* 8 (0x806c420) */
    beta_node_type_memory,
    &betas[7], 0, &betas[9], 
    0, 0,
    0, 0, { 0 } },
  { /* 9 (0x806c468) */
    beta_node_type_positive_join,
    &betas[8], 0, &betas[10], 
    &alphas[5], 0,
    0, 0, { &tests[4] } },
  { /* 10 (0x8070eb8) */
    beta_node_type_memory,
    &betas[9], 0, &betas[11], 
    0, 0,
    0, 0, { 0 } },
  { /* 11 (0x8070f00) */
    beta_node_type_positive_join,
    &betas[10], 0, &betas[12], 
    &alphas[4], 0,
    0, 0, { &tests[5] } },
  { /* 12 (0x8070f28) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 13 (0x806cdd0) */
    beta_node_type_memory,
    &betas[6], 0, &betas[14], 
    0, 0,
    0, 0, { 0 } },
  { /* 14 (0x806cdf8) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[15], 
    &alphas[10], &betas[6],
    0, 0, { &tests[7] } },
  { /* 15 (0x806ce30) */
    beta_node_type_memory,
    &betas[14], 0, &betas[16], 
    0, 0,
    0, 0, { 0 } },
  { /* 16 (0x806ce58) */
    beta_node_type_positive_join,
    &betas[15], 0, &betas[17], 
    &alphas[8], &betas[22],
    0, 0, { &tests[8] } },
  { /* 17 (0x806ce90) */
    beta_node_type_memory,
    &betas[16], 0, &betas[18], 
    0, 0,
    0, 0, { 0 } },
  { /* 18 (0x806ceb8) */
    beta_node_type_positive_join,
    &betas[17], 0, &betas[19], 
    &alphas[7], &betas[21],
    0, 0, { &tests[9] } },
  { /* 19 (0x806cee0) */
    beta_node_type_production,
    &betas[18], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
  { /* 20 (0x806ae98) */
    beta_node_type_memory,
    &betas[2], 0, &betas[21], 
    0, 0,
    0, 0, { 0 } },
  { /* 21 (0x806b0a8) */
    beta_node_type_positive_join,
    &betas[20], &betas[22], &betas[38], 
    &alphas[7], &betas[25],
    0, 0, { &tests[10] } },
  { /* 22 (0x806c728) */
    beta_node_type_positive_join,
    &betas[20], &betas[23], &betas[37], 
    &alphas[8], &betas[26],
    0, 0, { &tests[11] } },
  { /* 23 (0x806aec0) */
    beta_node_type_positive_join,
    &betas[20], 0, &betas[24], 
    &alphas[6], &betas[7],
    0, 0, { &tests[12] } },
  { /* 24 (0x806aef8) */
    beta_node_type_memory,
    &betas[23], 0, &betas[25], 
    0, 0,
    0, 0, { 0 } },
  { /* 25 (0x806b440) */
    beta_node_type_positive_join,
    &betas[24], &betas[26], &betas[30], 
    &alphas[7], 0,
    0, 0, { &tests[13] } },
  { /* 26 (0x806abe0) */
    beta_node_type_positive_join,
    &betas[24], 0, &betas[27], 
    &alphas[8], 0,
    0, 0, { &tests[14] } },
  { /* 27 (0x806ac28) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, { 0 } },
  { /* 28 (0x806c520) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[2], 0,
    0, 0, { &tests[15] } },
  { /* 29 (0x806c548) */
    beta_node_type_production,
    &betas[28], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[3] } },
  { /* 30 (0x806b468) */
    beta_node_type_memory,
    &betas[25], 0, &betas[31], 
    0, 0,
    0, 0, { 0 } },
  { /* 31 (0x806b490) */
    beta_node_type_positive_join,
    &betas[30], 0, &betas[32], 
    &alphas[2], &betas[28],
    0, 0, { &tests[16] } },
  { /* 32 (0x806b4d8) */
    beta_node_type_memory,
    &betas[31], 0, &betas[33], 
    0, 0,
    0, 0, { 0 } },
  { /* 33 (0x806b520) */
    beta_node_type_positive_join,
    &betas[32], 0, &betas[34], 
    &alphas[1], 0,
    0, 0, { &tests[17] } },
  { /* 34 (0x806c778) */
    beta_node_type_memory,
    &betas[33], 0, &betas[35], 
    0, 0,
    0, 0, { 0 } },
  { /* 35 (0x806c800) */
    beta_node_type_positive_join,
    &betas[34], 0, &betas[36], 
    &alphas[0], 0,
    0, 0, { &tests[18] } },
  { /* 36 (0x806c828) */
    beta_node_type_production,
    &betas[35], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[4] } },
  { /* 37 (0x806c750) */
    beta_node_type_production,
    &betas[22], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[5] } },
  { /* 38 (0x806b0d0) */
    beta_node_type_production,
    &betas[21], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[6] } },
};

static struct agent agent = {
  1, &betas[0], { &betas[0], 0, 0, 0 },
  { 0,
    0,
    &alphas[0],
    &alphas[7],
    0,
    0,
    0,
    0,
    0,
    0,
    &alphas[10],
    0,
    0,
    0,
    0,
    0 },
  0, 0, 0, 0, { 0, 0, 0, 0 }, 0 };

#endif /* DECL_RETE_NETWORK */
