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
  { /* 0 (0x8073588) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 25),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[30], 0 },
  { /* 1 (0x80734f8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 23),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[28], 0 },
  { /* 2 (0x806c848) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 13),
    DECLARE_NIL_SYMBOL,
    &alphas[3], &betas[26], 0 },
  { /* 3 (0x8054f68) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    &alphas[4], &betas[3], 0 },
  { /* 4 (0x806abc8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 20),
    DECLARE_NIL_SYMBOL,
    &alphas[5], &betas[11], 0 },
  { /* 5 (0x806ab38) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 5),
    DECLARE_NIL_SYMBOL,
    &alphas[6], &betas[9], 0 },
  { /* 6 (0x806cf00) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 6),
    DECLARE_NIL_SYMBOL,
    0, &betas[24], 0 },
  { /* 7 (0x806c9c8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 26),
    &alphas[8], &betas[37], 0 },
  { /* 8 (0x806ac80) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 22),
    &alphas[9], &betas[35], 0 },
  { /* 9 (0x806d170) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 16),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 9),
    0, &betas[4], 0 },
  { /* 10 (0x806cc98) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    0, &betas[33], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x806c990) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 1 (0x806afb8) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 2 (0x8071b98) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 3 (0x806cec8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x806ab00) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 5 (0x806ab90) */
    test_type_greater, relational_type_constant, field_value,
    { SYMBOL_TO_WORD(symbol_type_integer_constant, 512) }, 0 },
  { /* 6 (0x806ab80) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 7 (0x806cb90) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 8 (0x8054fb0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 9 (0x806acd8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 10 (0x806c810) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 11 (0x806cb50) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 2) }, 0 },
  { /* 12 (0x8073428) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 13 (0x80734c0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 14 (0x8073550) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 15 (0x80737e0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 16 (0x8073840) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 17 (0x8073850) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x806cba0) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x806cbf8) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 26) } },
    { 0, { 0 } } },
  { /* 2 (0x806d080) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 3 (0x806d100) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 22) } },
    { 0, { 0 } } },
  { /* 4 (0x806ca28) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 5 (0x806ae28) */
    &actions[6], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { 0, { 0 } } },
  { /* 6 (0x806af88) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 25) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 24) } },
    { 0, { 0 } } },
  { /* 7 (0x80736c0) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 8 (0x806caf0) */
    &actions[9], preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
  { /* 9 (0x806cb20) */
    0, preference_type_reject,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 25) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { 0, { 0 } } },
  { /* 10 (0x80738c0) */
    0, preference_type_better,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 3) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 3) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 2) } } },
};

static struct production productions[] = {
  { /* 0 (0x806caa8) */
#ifdef DEBUG
    "propose*wait",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x8055230) */
#ifdef DEBUG
    "propose*drive",
#endif
    0, &actions[2], 0, 1, support_type_isupport },
  { /* 2 (0x806c928) */
#ifdef DEBUG
    "drive*reconsider",
#endif
    0, &actions[4], 0, 0, support_type_osupport },
  { /* 3 (0x806ac38) */
#ifdef DEBUG
    "implement*drive",
#endif
    0, &actions[5], 0, 0, support_type_osupport },
  { /* 4 (0x806cce8) */
#ifdef DEBUG
    "reconsider*wait",
#endif
    0, &actions[7], 0, 0, support_type_osupport },
  { /* 5 (0x806cd20) */
#ifdef DEBUG
    "implement*wait",
#endif
    0, &actions[8], 0, 0, support_type_osupport },
  { /* 6 (0x806cc78) */
#ifdef DEBUG
    "prefer*drive-to-wait",
#endif
    0, &actions[10], 0, 0, support_type_isupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x80718f8) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x8070c98) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x80737b8) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[32], 
    &alphas[10], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x8054f88) */
    beta_node_type_positive_join,
    &betas[1], &betas[4], &betas[13], 
    &alphas[3], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x806ce90) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[5], 
    &alphas[9], 0,
    0, 0, { &tests[2] } },
  { /* 5 (0x806cc50) */
    beta_node_type_production,
    &betas[4], &betas[6], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 6 (0x806ced8) */
    beta_node_type_memory,
    &betas[4], 0, &betas[7], 
    0, 0,
    0, 0, { 0 } },
  { /* 7 (0x806aac8) */
    beta_node_type_positive_join,
    &betas[6], 0, &betas[8], 
    &alphas[6], 0,
    0, 0, { &tests[3] } },
  { /* 8 (0x806ab10) */
    beta_node_type_memory,
    &betas[7], 0, &betas[9], 
    0, 0,
    0, 0, { 0 } },
  { /* 9 (0x806ab58) */
    beta_node_type_positive_join,
    &betas[8], 0, &betas[10], 
    &alphas[5], 0,
    0, 0, { &tests[4] } },
  { /* 10 (0x806aba0) */
    beta_node_type_memory,
    &betas[9], 0, &betas[11], 
    0, 0,
    0, 0, { 0 } },
  { /* 11 (0x806abe8) */
    beta_node_type_positive_join,
    &betas[10], 0, &betas[12], 
    &alphas[4], 0,
    0, 0, { &tests[5] } },
  { /* 12 (0x806ac10) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 13 (0x806ac58) */
    beta_node_type_memory,
    &betas[3], 0, &betas[14], 
    0, 0,
    0, 0, { 0 } },
  { /* 14 (0x806c8b8) */
    beta_node_type_positive_join,
    &betas[13], &betas[15], &betas[22], 
    &alphas[7], 0,
    0, 0, { &tests[7] } },
  { /* 15 (0x806aca0) */
    beta_node_type_positive_join,
    &betas[13], 0, &betas[16], 
    &alphas[8], 0,
    0, 0, { &tests[8] } },
  { /* 16 (0x806ca80) */
    beta_node_type_production,
    &betas[15], &betas[17], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
  { /* 17 (0x806ace8) */
    beta_node_type_memory,
    &betas[15], 0, &betas[18], 
    0, 0,
    0, 0, { 0 } },
  { /* 18 (0x806c7d8) */
    beta_node_type_positive_join,
    &betas[17], 0, &betas[19], 
    &alphas[6], &betas[7],
    0, 0, { &tests[9] } },
  { /* 19 (0x806c820) */
    beta_node_type_memory,
    &betas[18], 0, &betas[20], 
    0, 0,
    0, 0, { 0 } },
  { /* 20 (0x806c868) */
    beta_node_type_positive_join,
    &betas[19], 0, &betas[21], 
    &alphas[2], 0,
    0, 0, { &tests[10] } },
  { /* 21 (0x806c890) */
    beta_node_type_production,
    &betas[20], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[3] } },
  { /* 22 (0x80735f8) */
    beta_node_type_production,
    &betas[14], &betas[23], 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[4] } },
  { /* 23 (0x80733c8) */
    beta_node_type_memory,
    &betas[14], 0, &betas[24], 
    0, 0,
    0, 0, { 0 } },
  { /* 24 (0x80733f0) */
    beta_node_type_positive_join,
    &betas[23], 0, &betas[25], 
    &alphas[6], &betas[18],
    0, 0, { &tests[11] } },
  { /* 25 (0x8073438) */
    beta_node_type_memory,
    &betas[24], 0, &betas[26], 
    0, 0,
    0, 0, { 0 } },
  { /* 26 (0x8073498) */
    beta_node_type_positive_join,
    &betas[25], 0, &betas[27], 
    &alphas[2], &betas[20],
    0, 0, { &tests[12] } },
  { /* 27 (0x80734d0) */
    beta_node_type_memory,
    &betas[26], 0, &betas[28], 
    0, 0,
    0, 0, { 0 } },
  { /* 28 (0x8073518) */
    beta_node_type_positive_join,
    &betas[27], 0, &betas[29], 
    &alphas[1], 0,
    0, 0, { &tests[13] } },
  { /* 29 (0x8073560) */
    beta_node_type_memory,
    &betas[28], 0, &betas[30], 
    0, 0,
    0, 0, { 0 } },
  { /* 30 (0x80735a8) */
    beta_node_type_positive_join,
    &betas[29], 0, &betas[31], 
    &alphas[0], 0,
    0, 0, { &tests[14] } },
  { /* 31 (0x80735d0) */
    beta_node_type_production,
    &betas[30], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[5] } },
  { /* 32 (0x80737f0) */
    beta_node_type_memory,
    &betas[2], 0, &betas[33], 
    0, 0,
    0, 0, { 0 } },
  { /* 33 (0x8073818) */
    beta_node_type_positive_join,
    &betas[32], 0, &betas[34], 
    &alphas[10], &betas[2],
    0, 0, { &tests[15] } },
  { /* 34 (0x8073918) */
    beta_node_type_memory,
    &betas[33], 0, &betas[35], 
    0, 0,
    0, 0, { 0 } },
  { /* 35 (0x8073940) */
    beta_node_type_positive_join,
    &betas[34], 0, &betas[36], 
    &alphas[8], &betas[15],
    0, 0, { &tests[16] } },
  { /* 36 (0x8073968) */
    beta_node_type_memory,
    &betas[35], 0, &betas[37], 
    0, 0,
    0, 0, { 0 } },
  { /* 37 (0x8073990) */
    beta_node_type_positive_join,
    &betas[36], 0, &betas[38], 
    &alphas[7], &betas[14],
    0, 0, { &tests[17] } },
  { /* 38 (0x80739b8) */
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
