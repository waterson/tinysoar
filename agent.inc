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
  { /* 0 (0x8074a68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(24, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[35], 0 },
  { /* 1 (0x80749e8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(23, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[33], 0 },
  { /* 2 (0x806c210) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(13, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[31], 0 },
  { /* 3 (0x806beb0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(12, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[2], 0 },
  { /* 4 (0x806b500) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(5, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[9], 0 },
  { /* 5 (0x806c978) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(6, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    0, &betas[23], 0 },
  { /* 6 (0x806c4f0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(25, symbol_type_symbolic_constant),
    &alphas[7], &betas[18], 0 },
  { /* 7 (0x806bf90) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(22, symbol_type_symbolic_constant),
    &alphas[8], &betas[16], 0 },
  { /* 8 (0x806b580) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(20, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(1023, symbol_type_integer_constant),
    &alphas[9], &betas[11], 0 },
  { /* 9 (0x806c908) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(16, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(9, symbol_type_symbolic_constant),
    0, &betas[3], 0 },
  { /* 10 (0x8074738) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(12, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    0, &betas[14], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x8072b60) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(0, symbol_type_identifier) }, 0 },
  { /* 1 (0x806a470) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(0, symbol_type_identifier) }, 0 },
  { /* 2 (0x8074728) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 3 (0x805eba0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x806c9d0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 5 (0x806b548) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 6 (0x8074cb0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 7 (0x8074d10) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 8 (0x8074d20) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 9 (0x806c510) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 10 (0x806c1b8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 11 (0x806bef8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 12 (0x806c128) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 13 (0x806bf58) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 14 (0x806bfe8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 15 (0x806c180) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 16 (0x8074868) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 17 (0x8074a30) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x8073018) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x8073070) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(25, symbol_type_symbolic_constant) } },
    { 0, { 0 } } },
  { /* 2 (0x8072b70) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 3 (0x806c870) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(22, symbol_type_symbolic_constant) } },
    { 0, { 0 } } },
  { /* 4 (0x8074dd0) */
    0, preference_type_better,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 4) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 3) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } } },
  { /* 5 (0x806be00) */
    &actions[6], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(100, symbol_type_integer_constant) } },
    { 0, { 0 } } },
  { /* 6 (0x806be58) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(100, symbol_type_integer_constant) } },
    { 0, { 0 } } },
  { /* 7 (0x8074960) */
    &actions[8], preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 8 (0x8074990) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { 0, { 0 } } },
  { /* 9 (0x8072f18) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 10 (0x8074b70) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
};

static struct production productions[] = {
  { /* 0 (0x8072fc0) */
#ifdef DEBUG
    "propose*wait",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x8072868) */
#ifdef DEBUG
    "propose*drive",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x8074b00) */
#ifdef DEBUG
    "prefer*drive-to-wait",
#endif
    0, &actions[4], 0, 0, support_type_isupport },
  { /* 3 (0x806c1c8) */
#ifdef DEBUG
    "implement*drive",
#endif
    0, &actions[5], 0, 0, support_type_osupport },
  { /* 4 (0x806c280) */
#ifdef DEBUG
    "implement*wait",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x806c198) */
#ifdef DEBUG
    "drive*reconsider",
#endif
    0, &actions[9], 0, 0, support_type_osupport },
  { /* 6 (0x8074798) */
#ifdef DEBUG
    "reconsider*wait",
#endif
    0, &actions[10], 0, 0, support_type_osupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x806e0b0) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x806c8c8) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x806bed0) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[20], 
    &alphas[3], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x806c928) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[4], 
    &alphas[9], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x806c4c8) */
    beta_node_type_production,
    &betas[3], &betas[5], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 5 (0x806c950) */
    beta_node_type_memory,
    &betas[3], 0, &betas[6], 
    0, 0,
    0, 0, { 0 } },
  { /* 6 (0x8074c78) */
    beta_node_type_positive_join,
    &betas[5], &betas[7], &betas[13], 
    &alphas[10], 0,
    0, 0, { &tests[2] } },
  { /* 7 (0x806c998) */
    beta_node_type_positive_join,
    &betas[5], 0, &betas[8], 
    &alphas[5], 0,
    0, 0, { &tests[3] } },
  { /* 8 (0x806c9e0) */
    beta_node_type_memory,
    &betas[7], 0, &betas[9], 
    0, 0,
    0, 0, { 0 } },
  { /* 9 (0x806b520) */
    beta_node_type_positive_join,
    &betas[8], 0, &betas[10], 
    &alphas[4], 0,
    0, 0, { &tests[4] } },
  { /* 10 (0x806b558) */
    beta_node_type_memory,
    &betas[9], 0, &betas[11], 
    0, 0,
    0, 0, { 0 } },
  { /* 11 (0x806b5a0) */
    beta_node_type_positive_join,
    &betas[10], 0, &betas[12], 
    &alphas[8], 0,
    0, 0, { &tests[5] } },
  { /* 12 (0x806c100) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 13 (0x8074cc0) */
    beta_node_type_memory,
    &betas[6], 0, &betas[14], 
    0, 0,
    0, 0, { 0 } },
  { /* 14 (0x8074ce8) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[15], 
    &alphas[10], &betas[6],
    0, 0, { &tests[6] } },
  { /* 15 (0x8074e28) */
    beta_node_type_memory,
    &betas[14], 0, &betas[16], 
    0, 0,
    0, 0, { 0 } },
  { /* 16 (0x8074e50) */
    beta_node_type_positive_join,
    &betas[15], 0, &betas[17], 
    &alphas[7], &betas[22],
    0, 0, { &tests[7] } },
  { /* 17 (0x8074e78) */
    beta_node_type_memory,
    &betas[16], 0, &betas[18], 
    0, 0,
    0, 0, { 0 } },
  { /* 18 (0x8074ea0) */
    beta_node_type_positive_join,
    &betas[17], 0, &betas[19], 
    &alphas[6], &betas[21],
    0, 0, { &tests[8] } },
  { /* 19 (0x8074ec8) */
    beta_node_type_production,
    &betas[18], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
  { /* 20 (0x806bf08) */
    beta_node_type_memory,
    &betas[2], 0, &betas[21], 
    0, 0,
    0, 0, { 0 } },
  { /* 21 (0x80747b8) */
    beta_node_type_positive_join,
    &betas[20], &betas[22], &betas[38], 
    &alphas[6], &betas[25],
    0, 0, { &tests[9] } },
  { /* 22 (0x8072f70) */
    beta_node_type_positive_join,
    &betas[20], &betas[23], &betas[37], 
    &alphas[7], &betas[26],
    0, 0, { &tests[10] } },
  { /* 23 (0x806bf30) */
    beta_node_type_positive_join,
    &betas[20], 0, &betas[24], 
    &alphas[5], &betas[7],
    0, 0, { &tests[11] } },
  { /* 24 (0x806bf68) */
    beta_node_type_memory,
    &betas[23], 0, &betas[25], 
    0, 0,
    0, 0, { 0 } },
  { /* 25 (0x80747f0) */
    beta_node_type_positive_join,
    &betas[24], &betas[26], &betas[30], 
    &alphas[6], 0,
    0, 0, { &tests[12] } },
  { /* 26 (0x806bfb0) */
    beta_node_type_positive_join,
    &betas[24], 0, &betas[27], 
    &alphas[7], 0,
    0, 0, { &tests[13] } },
  { /* 27 (0x806c1e8) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, { 0 } },
  { /* 28 (0x806c230) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[2], 0,
    0, 0, { &tests[14] } },
  { /* 29 (0x806c258) */
    beta_node_type_production,
    &betas[28], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[3] } },
  { /* 30 (0x8074818) */
    beta_node_type_memory,
    &betas[25], 0, &betas[31], 
    0, 0,
    0, 0, { 0 } },
  { /* 31 (0x8074840) */
    beta_node_type_positive_join,
    &betas[30], 0, &betas[32], 
    &alphas[2], &betas[28],
    0, 0, { &tests[15] } },
  { /* 32 (0x8074878) */
    beta_node_type_memory,
    &betas[31], 0, &betas[33], 
    0, 0,
    0, 0, { 0 } },
  { /* 33 (0x8074a08) */
    beta_node_type_positive_join,
    &betas[32], 0, &betas[34], 
    &alphas[1], 0,
    0, 0, { &tests[16] } },
  { /* 34 (0x8074a40) */
    beta_node_type_memory,
    &betas[33], 0, &betas[35], 
    0, 0,
    0, 0, { 0 } },
  { /* 35 (0x8074a88) */
    beta_node_type_positive_join,
    &betas[34], 0, &betas[36], 
    &alphas[0], 0,
    0, 0, { &tests[17] } },
  { /* 36 (0x8074ab0) */
    beta_node_type_production,
    &betas[35], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[4] } },
  { /* 37 (0x8072f98) */
    beta_node_type_production,
    &betas[22], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[5] } },
  { /* 38 (0x8074ad8) */
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
    &alphas[6],
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
