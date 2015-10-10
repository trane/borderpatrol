package com.lookout.borderpatrol.auth

/**
 * This provides the specification contracts for Keymaster auth.
 *
 * The composition of these filters should work, e.g.:
 *
 * val bpFilter = ServiceFilter andThen SessionIdFilter
 * val loginFilters = bpFilter andThen ...
 * val authFilters = bpFilter andThen IdentityFilter(???) andThen AccessFilter(???)
 */
package object keymaster
