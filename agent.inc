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
#define SYM_FORWARD 24
#define SYM_MOTOR_C 25
#define SYM_WAIT 26


#ifdef DECL_RETE_NETWORK

static struct beta_node betas[];

static struct alpha_node alphas[] = {
  { /* 0 (0x8055528) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 25),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[30], 0 },
  { /* 1 (0x80554a8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 23),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[28], 0 },
  { /* 2 (0x805cff8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 13),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[26], 0 },
  { /* 3 (0x80552c8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[3], 0 },
  { /* 4 (0x8054f20) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 20),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[11], 0 },
  { /* 5 (0x8054e90) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 5),
    DECLARE_NIL_SYMBOL,
    &alphas[6], &betas[9], 0 },
  { /* 6 (0x8054e00) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 6),
    DECLARE_NIL_SYMBOL,
    0, &betas[24], 0 },
  { /* 7 (0x8066c68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 26),
    &alphas[8], &betas[37], 0 },
  { /* 8 (0x8055320) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 22),
    &alphas[9], &betas[35], 0 },
  { /* 9 (0x8054d70) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 16),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 9),
    0, &betas[4], 0 },
  { /* 10 (0x8070628) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    0, &betas[33], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x8055938) */
    test_type_goal_id | relational_type_constant | field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 1 (0x80552b8) */
    test_type_goal_id | relational_type_constant | field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 2 (0x8054ba8) */
    test_type_goal_id | relational_type_constant | field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 3 (0x8054dc8) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x8054e58) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 5 (0x8054ee8) */
    test_type_greater | relational_type_constant | field_value,
    { SYMBOL_TO_WORD(symbol_type_integer_constant, 512) }, 0 },
  { /* 6 (0x8054ed8) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 7 (0x80553d8) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 8 (0x80552e8) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 9 (0x8055378) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 10 (0x805ce00) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 11 (0x8056da8) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 12 (0x8053ac0) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 13 (0x8053b30) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 14 (0x80554f0) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 15 (0x8070658) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 16 (0x80556b0) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 17 (0x8055710) */
    test_type_equality | relational_type_variable | field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x8054af0) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x8054b30) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 26) } },
    { 0, { 0 } } },
  { /* 2 (0x804ee08) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 3 (0x8054d00) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 22) } },
    { 0, { 0 } } },
  { /* 4 (0x80541b0) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 5 (0x8055100) */
    &actions[6], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { 0, { 0 } } },
  { /* 6 (0x8055260) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 25) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { 0, { 0 } } },
  { /* 7 (0x8055908) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 8 (0x8066be0) */
    &actions[9], preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 9 (0x8066c10) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 25) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { 0, { 0 } } },
  { /* 10 (0x80705d0) */
    0, preference_type_better,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 3) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } } },
};

static struct production productions[] = {
  { /* 0 (0x806f210) */
#ifdef DEBUG
    "propose*wait",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x8051dd8) */
#ifdef DEBUG
    "propose*drive",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x805cf60) */
#ifdef DEBUG
    "drive*reconsider",
#endif
    0, &actions[4], 0, 0, support_type_osupport },
  { /* 3 (0x8054f90) */
#ifdef DEBUG
    "implement*drive",
#endif
    0, &actions[5], 0, 0, support_type_osupport },
  { /* 4 (0x8055818) */
#ifdef DEBUG
    "reconsider*wait",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x8063038) */
#ifdef DEBUG
    "implement*wait",
#endif
    0, &actions[8], 0, 0, support_type_osupport },
  { /* 6 (0x8055688) */
#ifdef DEBUG
    "prefer*drive-to-wait",
#endif
    0, &actions[10], 0, 0, support_type_isupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x806fa58) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x8052148) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x8055840) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[32], 
    &alphas[10], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x8054fb8) */
    beta_node_type_positive_join,
    &betas[1], &betas[4], &betas[13], 
    &alphas[3], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x8054d90) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[5], 
    &alphas[9], 0,
    0, 0, { &tests[2] } },
  { /* 5 (0x806f238) */
    beta_node_type_production,
    &betas[4], &betas[6], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 6 (0x8054dd8) */
    beta_node_type_memory,
    &betas[4], 0, &betas[7], 
    0, 0,
    0, 0, { 0 } },
  { /* 7 (0x8054e20) */
    beta_node_type_positive_join,
    &betas[6], 0, &betas[8], 
    &alphas[6], 0,
    0, 0, { &tests[3] } },
  { /* 8 (0x8054e68) */
    beta_node_type_memory,
    &betas[7], 0, &betas[9], 
    0, 0,
    0, 0, { 0 } },
  { /* 9 (0x8054eb0) */
    beta_node_type_positive_join,
    &betas[8], 0, &betas[10], 
    &alphas[5], 0,
    0, 0, { &tests[4] } },
  { /* 10 (0x8054ef8) */
    beta_node_type_memory,
    &betas[9], 0, &betas[11], 
    0, 0,
    0, 0, { 0 } },
  { /* 11 (0x8054f40) */
    beta_node_type_positive_join,
    &betas[10], 0, &betas[12], 
    &alphas[4], 0,
    0, 0, { &tests[5] } },
  { /* 12 (0x8054f68) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 13 (0x80552f8) */
    beta_node_type_memory,
    &betas[3], 0, &betas[14], 
    0, 0,
    0, 0, { 0 } },
  { /* 14 (0x8063060) */
    beta_node_type_positive_join,
    &betas[13], &betas[15], &betas[22], 
    &alphas[7], 0,
    0, 0, { &tests[7] } },
  { /* 15 (0x8055340) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[16], 
    &alphas[8], 0,
    0, 0, { &tests[8] } },
  { /* 16 (0x805cc28) */
    beta_node_type_production,
    &betas[15], &betas[17], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
  { /* 17 (0x8055388) */
    beta_node_type_memory,
    &betas[15], 0, &betas[18], 
    0, 0,
    0, 0, { 0 } },
  { /* 18 (0x80553b0) */
    beta_node_type_positive_join,
    &betas[17], 0, &betas[19], 
    &alphas[6], &betas[7],
    0, 0, { &tests[9] } },
  { /* 19 (0x805ce10) */
    beta_node_type_memory,
    &betas[18], 0, &betas[20], 
    0, 0,
    0, 0, { 0 } },
  { /* 20 (0x805d018) */
    beta_node_type_positive_join,
    &betas[19], 0, &betas[21], 
    &alphas[2], 0,
    0, 0, { &tests[10] } },
  { /* 21 (0x805cf38) */
    beta_node_type_production,
    &betas[20], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[3] } },
  { /* 22 (0x8055660) */
    beta_node_type_production,
    &betas[14], &betas[23], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[4] } },
  { /* 23 (0x8066c88) */
    beta_node_type_memory,
    &betas[14], 0, &betas[24], 
    0, 0,
    0, 0, { 0 } },
  { /* 24 (0x8066cb0) */
    beta_node_type_positive_join,
    &betas[23], 0, &betas[25], 
    &alphas[6], &betas[18],
    0, 0, { &tests[11] } },
  { /* 25 (0x8053ad0) */
    beta_node_type_memory,
    &betas[24], 0, &betas[26], 
    0, 0,
    0, 0, { 0 } },
  { /* 26 (0x8053af8) */
    beta_node_type_positive_join,
    &betas[25], 0, &betas[27], 
    &alphas[2], &betas[20],
    0, 0, { &tests[12] } },
  { /* 27 (0x8055480) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, { 0 } },
  { /* 28 (0x80554c8) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[1], 0,
    0, 0, { &tests[13] } },
  { /* 29 (0x8055500) */
    beta_node_type_memory,
    &betas[28], 0, &betas[30], 
    0, 0,
    0, 0, { 0 } },
  { /* 30 (0x8055548) */
    beta_node_type_positive_join,
    &betas[29], 0, &betas[31], 
    &alphas[0], 0,
    0, 0, { &tests[14] } },
  { /* 31 (0x8055570) */
    beta_node_type_production,
    &betas[30], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[5] } },
  { /* 32 (0x8070668) */
    beta_node_type_memory,
    &betas[2], 0, &betas[33], 
    0, 0,
    0, 0, { 0 } },
  { /* 33 (0x8070690) */
    beta_node_type_positive_join,
    &betas[32], 0, &betas[34], 
    &alphas[10], &betas[2],
    0, 0, { &tests[15] } },
  { /* 34 (0x80556c0) */
    beta_node_type_memory,
    &betas[33], 0, &betas[35], 
    0, 0,
    0, 0, { 0 } },
  { /* 35 (0x80556e8) */
    beta_node_type_positive_join,
    &betas[34], 0, &betas[36], 
    &alphas[8], &betas[15],
    0, 0, { &tests[16] } },
  { /* 36 (0x8055720) */
    beta_node_type_memory,
    &betas[35], 0, &betas[37], 
    0, 0,
    0, 0, { 0 } },
  { /* 37 (0x8057e90) */
    beta_node_type_positive_join,
    &betas[36], 0, &betas[38], 
    &alphas[7], &betas[14],
    0, 0, { &tests[17] } },
  { /* 38 (0x8057eb8) */
    beta_node_type_production,
    &betas[37], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[6] } },
};

static struct agent agent = {
  0, 0, 0,
  &betas[0], { &betas[0], 0, 0, 0 },
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
  0, 0, 0, { 0, 0, 0, 0 }, 0 };

#endif /* DECL_RETE_NETWORK */
