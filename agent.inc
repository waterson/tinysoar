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
#define SYM_NAME 20
#define SYM_DRIVE 21
#define SYM_MOTOR_A 22
#define SYM_MOTOR_C 23


#ifdef DECL_RETE_NETWORK

static struct beta_node betas[];

static struct alpha_node alphas[] = {
  { /* 0 (0x8071b20) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 13),
    DECLARE_NIL_SYMBOL,
    &alphas[1], &betas[11], 0 },
  { /* 1 (0x8071d98) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 6),
    DECLARE_NIL_SYMBOL,
    &alphas[2], &betas[7], 0 },
  { /* 2 (0x8071d30) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 12),
    DECLARE_NIL_SYMBOL,
    0, &betas[2], 0 },
  { /* 3 (0x8071aa0) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 20),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 21),
    &alphas[4], &betas[6], 0 },
  { /* 4 (0x806c1e8) */
    DECLARE_NIL_SYMBOL,
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 16),
    DECLARE_SYMBOL(symbol_type_symbolic_constant, 9),
    0, &betas[3], 0 },
};

static struct beta_test tests[] = {
  { /* 0 (0x806c258) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 1 (0x806c1c0) */
    test_type_goal_id, relational_type_constant, field_id,
    { SYMBOL_TO_WORD(symbol_type_identifier, 0) }, 0 },
  { /* 2 (0x806bcc8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 1) }, 0 },
  { /* 3 (0x8071d60) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_id, 1) }, 0 },
  { /* 4 (0x8071de0) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
  { /* 5 (0x8071ae8) */
    test_type_equality, relational_type_variable, field_id,
    { VARIABLE_BINDING_TO_WORD(field_value, 2) }, 0 },
};

static struct action actions[] = {
  { /* 0 (0x806c0e8) */
    &actions[1], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_unbound_variable, { 0 } },
    { 0, { 0 } } },
  { /* 1 (0x806c168) */
    0, preference_type_acceptable,
    { rhs_value_type_unbound_variable, { 0 } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 20) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 21) } },
    { 0, { 0 } } },
  { /* 2 (0x8071c80) */
    &actions[3], preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 22) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_integer_constant, 100) } },
    { 0, { 0 } } },
  { /* 3 (0x8071cd8) */
    0, preference_type_acceptable,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 0) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 23) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_integer_constant, 100) } },
    { 0, { 0 } } },
  { /* 4 (0x806bc70) */
    0, preference_type_reconsider,
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_id, 1) } },
    { rhs_value_type_symbol, { SYMBOL_TO_WORD(symbol_type_symbolic_constant, 12) } },
    { rhs_value_type_variable_binding, { VARIABLE_BINDING_TO_WORD(field_value, 1) } },
    { 0, { 0 } } },
};

static struct production productions[] = {
  { /* 0 (0x8052ed0) */
#ifdef DEBUG
    "propose*drive",
#endif
    0, &actions[0], 0, 1, support_type_isupport },
  { /* 1 (0x806c040) */
#ifdef DEBUG
    "implement*drive",
#endif
    0, &actions[2], 0, 0, support_type_osupport },
  { /* 2 (0x806bb88) */
#ifdef DEBUG
    "drive*reconsider",
#endif
    0, &actions[4], 0, 0, support_type_osupport },
};

static struct agent agent;

static struct beta_node betas[] = {
  { /* 0 (0x80709b8) */
    beta_node_type_root,
    0, 0, &betas[1], 
    0, 0,
    &agent.root_token, 0, { 0 } },
  { /* 1 (0x8070a00) */
    beta_node_type_memory,
    &betas[0], 0, &betas[2], 
    0, 0,
    0, 0, { 0 } },
  { /* 2 (0x806c060) */
    beta_node_type_positive_join,
    &betas[1], &betas[3], &betas[5], 
    &alphas[2], 0,
    0, 0, { &tests[0] } },
  { /* 3 (0x806c208) */
    beta_node_type_positive_join,
    &betas[1], 0, &betas[4], 
    &alphas[4], 0,
    0, 0, { &tests[1] } },
  { /* 4 (0x806c230) */
    beta_node_type_production,
    &betas[3], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[0] } },
  { /* 5 (0x8071d70) */
    beta_node_type_memory,
    &betas[2], 0, &betas[6], 
    0, 0,
    0, 0, { 0 } },
  { /* 6 (0x806bcd8) */
    beta_node_type_positive_join,
    &betas[5], &betas[7], &betas[13], 
    &alphas[3], &betas[9],
    0, 0, { &tests[2] } },
  { /* 7 (0x8071db8) */
    beta_node_type_positive_join,
    &betas[5], 0, &betas[8], 
    &alphas[1], 0,
    0, 0, { &tests[3] } },
  { /* 8 (0x8071df0) */
    beta_node_type_memory,
    &betas[7], 0, &betas[9], 
    0, 0,
    0, 0, { 0 } },
  { /* 9 (0x8071ac0) */
    beta_node_type_positive_join,
    &betas[8], 0, &betas[10], 
    &alphas[3], 0,
    0, 0, { &tests[4] } },
  { /* 10 (0x8071af8) */
    beta_node_type_memory,
    &betas[9], 0, &betas[11], 
    0, 0,
    0, 0, { 0 } },
  { /* 11 (0x806bac8) */
    beta_node_type_positive_join,
    &betas[10], 0, &betas[12], 
    &alphas[0], 0,
    0, 0, { &tests[5] } },
  { /* 12 (0x806baf0) */
    beta_node_type_production,
    &betas[11], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[1] } },
  { /* 13 (0x806bd00) */
    beta_node_type_production,
    &betas[6], 0, 0, 
    0, 0,
    0, 0, { (struct beta_test *) &productions[2] } },
};

static struct agent agent = {
  1, &betas[0], { &betas[0], 0, 0, 0 },
  { 0,
    0,
    &alphas[0],
    &alphas[3],
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
