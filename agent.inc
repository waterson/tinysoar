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
  { /* 0 (0x8073890) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(24, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[28], 0 },
  { /* 1 (0x8073810) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(23, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[26], 0 },
  { /* 2 (0x806aeb0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(25, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[21], 0 },
  { /* 3 (0x806d2f0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(5, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[17], 0 },
  { /* 4 (0x806c980) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(13, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[24], 0 },
  { /* 5 (0x8070bc8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(6, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    &alphas[6], &betas[12], 0 },
  { /* 6 (0x806cfa0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(12, symbol_type_symbolic_constant),
    DECLARE_NIL_SYMBOL,
    0, &betas[2], 0 },
  { /* 7 (0x806af68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(26, symbol_type_symbolic_constant),
    &alphas[8], &betas[10], 0 },
  { /* 8 (0x8070c38) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(21, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(22, symbol_type_symbolic_constant),
    &alphas[9], &betas[11], 0 },
  { /* 9 (0x806d1d0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(20, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(19, symbol_type_symbolic_constant),
    &alphas[10], &betas[7], 0 },
  { /* 10 (0x806d178) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(16, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(9, symbol_type_symbolic_constant),
    0, &betas[3], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x80550b0) */
    test_type_goal_id, relational_type_constant, field_id,
    SYMBOL_TO_WORD(0, symbol_type_identifier) },
  { /* 1 (0x8070de8) */
    test_type_goal_id, relational_type_constant, field_id,
    SYMBOL_TO_WORD(0, symbol_type_identifier) },
  { /* 2 (0x806d1c0) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_id, 1) },
  { /* 3 (0x806ce90) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_id, 1) },
  { /* 4 (0x806ca40) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 1) },
  { /* 5 (0x8070ec0) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 1) },
  { /* 6 (0x806cfe8) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_id, 1) },
  { /* 7 (0x806b018) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
  { /* 8 (0x806d020) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
  { /* 9 (0x806ae30) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
  { /* 10 (0x8070c90) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
  { /* 11 (0x806ae78) */
    test_type_greater, relational_type_constant, field_value,
    SYMBOL_TO_WORD(75, symbol_type_integer_constant) },
  { /* 12 (0x806ae68) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 1) },
  { /* 13 (0x8073620) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
  { /* 14 (0x8073690) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 1) },
  { /* 15 (0x8073858) */
    test_type_equality, relational_type_variable, field_id,
    VARIABLE_BINDING_TO_WORD(field_value, 2) },
};

static struct action actions[] = {
  { /* 0 (0x8070d68) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_id, 1) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) },
    { rhs_value_type_unbound_variable, 0 },
    { 0, 0 } },
  { /* 1 (0x806d0e0) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, 0 },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(22, symbol_type_symbolic_constant) },
    { 0, 0 } },
  { /* 2 (0x806ca50) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_id, 1) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) },
    { rhs_value_type_unbound_variable, 0 },
    { 0, 0 } },
  { /* 3 (0x806cad0) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, 0 },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(21, symbol_type_symbolic_constant) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(26, symbol_type_symbolic_constant) },
    { 0, 0 } },
  { /* 4 (0x806cef0) */
    &actions[5], preference_type_acceptable,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 0) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(100, symbol_type_integer_constant) },
    { 0, 0 } },
  { /* 5 (0x806cf48) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 0) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(100, symbol_type_integer_constant) },
    { 0, 0 } },
  { /* 6 (0x806add8) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_id, 4) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(20, symbol_type_symbolic_constant) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(19, symbol_type_symbolic_constant) },
    { 0, 0 } },
  { /* 7 (0x8073788) */
    &actions[8], preference_type_reject,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 2) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(23, symbol_type_symbolic_constant) },
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 1) },
    { 0, 0 } },
  { /* 8 (0x80737b8) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 2) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(24, symbol_type_symbolic_constant) },
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 0) },
    { 0, 0 } },
  { /* 9 (0x806b0a8) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_id, 1) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) },
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 1) },
    { 0, 0 } },
  { /* 10 (0x8073998) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_id, 1) },
    { rhs_value_type_symbol, SYMBOL_TO_WORD(12, symbol_type_symbolic_constant) },
    { rhs_value_type_variable_binding, VARIABLE_BINDING_TO_WORD(field_value, 1) },
    { 0, 0 } },
};

static struct production productions[] = {
  { /* 0 (0x8054c98) */
#ifdef DEBUG
    "propose*find-line",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x806b470) */
#ifdef DEBUG
    "propose*wait",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x8071e60) */
#ifdef DEBUG
    "implement*find-line*move-forward",
#endif
    0, &actions[4], 0, 0, support_type_osupport },
  { /* 3 (0x806cab0) */
#ifdef DEBUG
    "implement*find-line*detect-line",
#endif
    0, &actions[6], 0, 0, support_type_osupport },
  { /* 4 (0x806cba0) */
#ifdef DEBUG
    "implement*wait",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x806af98) */
#ifdef DEBUG
    "find-line*reconsider",
#endif
    0, &actions[9], 0, 0, support_type_osupport },
  { /* 6 (0x80735b8) */
#ifdef DEBUG
    "reconsider*wait",
#endif
    0, &actions[10], 0, 0, support_type_osupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x8070f80) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, 0 },
  { /* 1 (0x806d138) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, 0 },
  { /* 2 (0x806cfc0) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[9], 
    &alphas[6], 0,
    0, 0, &tests[0] },
  { /* 3 (0x806d198) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[4], 
    &alphas[10], 0,
    0, 0, &tests[1] },
  { /* 4 (0x806cb28) */
    beta_node_type_memory,
    &betas[3], &betas[5], &betas[7], 
    0, 0,
    0, 0, 0 },
  { /* 5 (0x806d1f0) */
    beta_node_type_negative,
    &betas[3], 0, &betas[6], 
    &alphas[9], 0,
    0, 0, &tests[2] },
  { /* 6 (0x806d218) */
    beta_node_type_production,
    &betas[5], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[0] },
  { /* 7 (0x806cb50) */
    beta_node_type_positive_join,
    &betas[4], 0, &betas[8], 
    &alphas[9], &betas[5],
    0, 0, &tests[3] },
  { /* 8 (0x806cb78) */
    beta_node_type_production,
    &betas[7], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[1] },
  { /* 9 (0x806cff8) */
    beta_node_type_memory,
    &betas[2], 0, &betas[10], 
    0, 0,
    0, 0, 0 },
  { /* 10 (0x80735d8) */
    beta_node_type_positive_join,
    &betas[9], &betas[11], &betas[31], 
    &alphas[7], &betas[14],
    0, 0, &tests[4] },
  { /* 11 (0x8070ed0) */
    beta_node_type_positive_join,
    &betas[9], &betas[12], &betas[30], 
    &alphas[8], &betas[15],
    0, 0, &tests[5] },
  { /* 12 (0x8070be8) */
    beta_node_type_positive_join,
    &betas[9], 0, &betas[13], 
    &alphas[5], 0,
    0, 0, &tests[6] },
  { /* 13 (0x8070c10) */
    beta_node_type_memory,
    &betas[12], 0, &betas[14], 
    0, 0,
    0, 0, 0 },
  { /* 14 (0x806af20) */
    beta_node_type_positive_join,
    &betas[13], &betas[15], &betas[23], 
    &alphas[7], 0,
    0, 0, &tests[7] },
  { /* 15 (0x8070c58) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[16], 
    &alphas[8], 0,
    0, 0, &tests[8] },
  { /* 16 (0x806c958) */
    beta_node_type_memory,
    &betas[15], 0, &betas[17], 
    0, 0,
    0, 0, 0 },
  { /* 17 (0x806ae40) */
    beta_node_type_positive_join,
    &betas[16], &betas[18], &betas[20], 
    &alphas[3], 0,
    0, 0, &tests[9] },
  { /* 18 (0x806c9a0) */
    beta_node_type_positive_join,
    &betas[16], 0, &betas[19], 
    &alphas[4], 0,
    0, 0, &tests[10] },
  { /* 19 (0x806c9c8) */
    beta_node_type_production,
    &betas[18], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[2] },
  { /* 20 (0x806ae88) */
    beta_node_type_memory,
    &betas[17], 0, &betas[21], 
    0, 0,
    0, 0, 0 },
  { /* 21 (0x806aed0) */
    beta_node_type_positive_join,
    &betas[20], 0, &betas[22], 
    &alphas[2], 0,
    0, 0, &tests[11] },
  { /* 22 (0x806aef8) */
    beta_node_type_production,
    &betas[21], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[3] },
  { /* 23 (0x8073630) */
    beta_node_type_memory,
    &betas[14], 0, &betas[24], 
    0, 0,
    0, 0, 0 },
  { /* 24 (0x8073658) */
    beta_node_type_positive_join,
    &betas[23], 0, &betas[25], 
    &alphas[4], &betas[18],
    0, 0, &tests[13] },
  { /* 25 (0x80736a0) */
    beta_node_type_memory,
    &betas[24], 0, &betas[26], 
    0, 0,
    0, 0, 0 },
  { /* 26 (0x8073830) */
    beta_node_type_positive_join,
    &betas[25], 0, &betas[27], 
    &alphas[1], 0,
    0, 0, &tests[14] },
  { /* 27 (0x8073868) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, 0 },
  { /* 28 (0x80738b0) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[0], 0,
    0, 0, &tests[15] },
  { /* 29 (0x80738d8) */
    beta_node_type_production,
    &betas[28], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[4] },
  { /* 30 (0x8070ef8) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[5] },
  { /* 31 (0x8073900) */
    beta_node_type_production,
    &betas[10], 0, 0, 
    0, 0,
    0, 0, (struct beta_test*) &productions[6] },
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
