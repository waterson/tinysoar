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
#define SYM_FOUND_LINE 20
#define SYM_NAME 21
#define SYM_FIND_LINE 22
#define SYM_MOTOR_A 23
#define SYM_MOTOR_C 24
#define SYM_SENSOR_A 25
#define SYM_WAIT 26


#ifdef DECL_RETE_NETWORK

static struct beta_node betas[];

static struct alpha_node alphas[] = {
  { /* 0 (0x80736d8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(24, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[28], 0 },
  { /* 1 (0x806ae90) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(23, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[26], 0 },
  { /* 2 (0x806ae00) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(25, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[21], 0 },
  { /* 3 (0x806ad70) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(5, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[17], 0 },
  { /* 4 (0x8071ef8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(13, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[24], 0 },
  { /* 5 (0x806cb08) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(6, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[6], &betas[12], 0 },
  { /* 6 (0x806ca78) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(12, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    0, &betas[2], 0 },
  { /* 7 (0x806ae70) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(26, symbol_type_symbolic_constant),
    &alphas[8], &betas[10], 0 },
  { /* 8 (0x8071e68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(22, symbol_type_symbolic_constant),
    &alphas[9], &betas[11], 0 },
  { /* 9 (0x8070c20) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(20, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(19, symbol_type_symbolic_constant),
    &alphas[10], &betas[7], 0 },
  { /* 10 (0x8070bc8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(16, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(9, symbol_type_symbolic_constant),
    0, &betas[3], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x8053210) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(0, symbol_type_identifier) }, 0 },
  { /* 1 (0x80718d0) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(0, symbol_type_identifier) }, 0 },
  { /* 2 (0x8070c10) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 3 (0x806c968) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x806af28) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 5 (0x8054ed0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 6 (0x806cad0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 7 (0x8070cd8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 8 (0x806cb50) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 9 (0x806ad60) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 10 (0x8071ec0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 11 (0x806adc8) */
    test_type_greater, relational_type_constant, field_value,
    { SYMBOL_TO_WORD(75, symbol_type_integer_constant) }, 0 },
  { /* 12 (0x806adb8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 13 (0x80734b0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 14 (0x8073520) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 15 (0x8073540) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x8071fd0) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x8070b58) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(22, symbol_type_symbolic_constant) } },
    { 0, { 0 } } },
  { /* 2 (0x806af38) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 3 (0x806cd78) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(26, symbol_type_symbolic_constant) } },
    { 0, { 0 } } },
  { /* 4 (0x806c9c8) */
    &actions[5], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(100, symbol_type_integer_constant) } },
    { 0, { 0 } } },
  { /* 5 (0x806ca20) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(100, symbol_type_integer_constant) } },
    { 0, { 0 } } },
  { /* 6 (0x806ad08) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 4) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(20, symbol_type_symbolic_constant) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(19, symbol_type_symbolic_constant) } },
    { 0, { 0 } } },
  { /* 7 (0x80735d8) */
    &actions[8], preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 8 (0x8073608) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { 0, { 0 } } },
  { /* 9 (0x806afd0) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 10 (0x80737b8) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
};

static struct production productions[] = {
  { /* 0 (0x8057a80) */
#ifdef DEBUG
    "",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x8054f30) */
#ifdef DEBUG
    "",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x8071e28) */
#ifdef DEBUG
    "",
#endif
    0, &actions[4], 0, 0, support_type_osupport },
  { /* 3 (0x80716d0) */
#ifdef DEBUG
    "",
#endif
    0, &actions[6], 0, 0, support_type_osupport },
  { /* 4 (0x806af90) */
#ifdef DEBUG
    "",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x806aee8) */
#ifdef DEBUG
    "",
#endif
    0, &actions[9], 0, 0, support_type_osupport },
  { /* 6 (0x806ceb8) */
#ifdef DEBUG
    "",
#endif
    0, &actions[10], 0, 0, support_type_osupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x8070d58) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x806b2e8) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x806ca98) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[9], 
    &alphas[6], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x8070be8) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[4], 
    &alphas[10], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x806cdd0) */
    beta_node_type_memory,
    &betas[3], &betas[5], &betas[7], 
    0, 0,
    0, 0, { 0 } },
  { /* 5 (0x8070c40) */
    beta_node_type_negative,
    &betas[3], 0, &betas[6], 
    &alphas[9], 0,
    0, 0, { &tests[2] } },
  { /* 6 (0x8070c68) */
    beta_node_type_production,
    &betas[5], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 7 (0x806cdf8) */
    beta_node_type_positive_join,
    &betas[4], 0, &betas[8], 
    &alphas[9], &betas[5],
    0, 0, { &tests[3] } },
  { /* 8 (0x806ce20) */
    beta_node_type_production,
    &betas[7], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 9 (0x806cae0) */
    beta_node_type_memory,
    &betas[2], 0, &betas[10], 
    0, 0,
    0, 0, { 0 } },
  { /* 10 (0x806ced0) */
    beta_node_type_positive_join,
    &betas[9], &betas[11], &betas[31], 
    &alphas[7], &betas[14],
    0, 0, { &tests[4] } },
  { /* 11 (0x8054ee0) */
    beta_node_type_positive_join,
    &betas[9], &betas[12], &betas[30], 
    &alphas[8], &betas[15],
    0, 0, { &tests[5] } },
  { /* 12 (0x806cb28) */
    beta_node_type_positive_join,
    &betas[9], 0, &betas[13], 
    &alphas[5], 0,
    0, 0, { &tests[6] } },
  { /* 13 (0x8071e40) */
    beta_node_type_memory,
    &betas[12], 0, &betas[14], 
    0, 0,
    0, 0, { 0 } },
  { /* 14 (0x8073488) */
    beta_node_type_positive_join,
    &betas[13], &betas[15], &betas[23], 
    &alphas[7], 0,
    0, 0, { &tests[7] } },
  { /* 15 (0x8071e88) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[16], 
    &alphas[8], 0,
    0, 0, { &tests[8] } },
  { /* 16 (0x8071ed0) */
    beta_node_type_memory,
    &betas[15], 0, &betas[17], 
    0, 0,
    0, 0, { 0 } },
  { /* 17 (0x806ad90) */
    beta_node_type_positive_join,
    &betas[16], &betas[18], &betas[20], 
    &alphas[3], 0,
    0, 0, { &tests[9] } },
  { /* 18 (0x806cd28) */
    beta_node_type_positive_join,
    &betas[16], 0, &betas[19], 
    &alphas[4], 0,
    0, 0, { &tests[10] } },
  { /* 19 (0x806cd50) */
    beta_node_type_production,
    &betas[18], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
  { /* 20 (0x806add8) */
    beta_node_type_memory,
    &betas[17], 0, &betas[21], 
    0, 0,
    0, 0, { 0 } },
  { /* 21 (0x806ae20) */
    beta_node_type_positive_join,
    &betas[20], 0, &betas[22], 
    &alphas[2], 0,
    0, 0, { &tests[11] } },
  { /* 22 (0x806ae48) */
    beta_node_type_production,
    &betas[21], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[3] } },
  { /* 23 (0x80734c0) */
    beta_node_type_memory,
    &betas[14], 0, &betas[24], 
    0, 0,
    0, 0, { 0 } },
  { /* 24 (0x80734e8) */
    beta_node_type_positive_join,
    &betas[23], 0, &betas[25], 
    &alphas[4], &betas[18],
    0, 0, { &tests[13] } },
  { /* 25 (0x8073660) */
    beta_node_type_memory,
    &betas[24], 0, &betas[26], 
    0, 0,
    0, 0, { 0 } },
  { /* 26 (0x8073688) */
    beta_node_type_positive_join,
    &betas[25], 0, &betas[27], 
    &alphas[1], 0,
    0, 0, { &tests[14] } },
  { /* 27 (0x80736b0) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, { 0 } },
  { /* 28 (0x80736f8) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[0], 0,
    0, 0, { &tests[15] } },
  { /* 29 (0x8073720) */
    beta_node_type_production,
    &betas[28], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[4] } },
  { /* 30 (0x8054f08) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[5] } },
  { /* 31 (0x8073748) */
    beta_node_type_production,
    &betas[10], 0, 0, 
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
    0,
    0,
    0,
    0,
    0,
    0 },
  0, 0, 0, 0, { 0, 0, 0, 0 }, 0 };

#endif /* DECL_RETE_NETWORK */
