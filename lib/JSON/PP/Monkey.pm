
package JSON::PP::Monkey;

use 5.10.1;
use strict;
use warnings;

use JSON::PP 2.94;
use parent qw(JSON::PP);

use Carp ();
use Scalar::Util qw(blessed refaddr reftype);

BEGIN { *P_ALLOW_UNKNOWN = *JSON::PP::P_ALLOW_UNKNOWN }
BEGIN { *_is_bignum = *JSON::PP::_is_bignum }
BEGIN { *_looks_like_number = *JSON::PP::_looks_like_number }

sub add_fallback {
    my ($self, $case, $cb) = @_;
    push @{$self->{fallbacks}{$case}}, $cb;
    return $self;
}

sub remove_fallback {
    my ($self, $case, $cb) = @_;

    if ($cb) {
        $self->{fallbacks}{$case} = [grep { $cb ne $_ } @{$self->{fallbacks}{$case}}];
        delete $self->{fallbacks}{$case} unless @{$self->{fallbacks}{$case}};
    }

    return $self;
}

sub _emit_fallback_to_json {
    my ($self, $case, $value) = @_;

    if (my $s = $self->{fallbacks}{$case}) {
        for my $cb (@$s) {
            if (my ($r) = $self->$cb($value, $case)) {

                if ( defined $r and ref( $r ) ) {
                    if ( refaddr( $value ) eq refaddr( $r ) ) {
                        encode_error( sprintf(
                            "'%s' fallback (%s) returned same object as was passed instead of a new one",
                            $case, $cb
                        ) );
                    }
                }

                return $self->object_to_json($r);
            }
        }
    }
    return 'null' if $self->_get_collapse_to_null($case);
    return;
}

# $json->_collapse_to_null($case, $enable);
sub _collapse_to_null { $_[0]->{"collapse_$_[1]"} = @_ > 2 ? !!$_[2] : !!1; $_[0] }
sub _get_collapse_to_null { $_[0]->{"collapse_$_[1]"} // !!1 }

sub collapse_blessed { shift->_collapse_to_null( 'blessed', @_ ) }
sub get_collapse_blessed { $_[0]->_get_collapse_to_null('blessed') }

sub collapse_unknown { shift->_collapse_to_null( 'unknown', @_ ) }
sub get_collapse_unknown { $_[0]->_get_collapse_to_null('unknown') }

# Helpful fallbacks

sub _convert_bignum {
    return unless _is_bignum($_[1]);
    return "$_[1]";
}

sub _convert_as_nonblessed {    # Based on JSON::PP::blessed_to_json
    my $obj = $_[1];
    my $reftype = reftype($obj) || '';
    if ($reftype eq 'HASH') {
        return {%$obj};
    }
    elsif ($reftype eq 'ARRAY') {
        return [@$obj];
    }
    else {
        return undef;
    }
}

sub _convert_with_to_json {
    return unless $_[1]->can('TO_JSON');
    return $_[1]->TO_JSON;
}

# Methods 'convert_bignum' and 'convert_as_nonblessed'

BEGIN {
   my @hidden_properties = qw( allow_bignum as_nonblessed );
   for my $name (@hidden_properties) {
       no strict 'refs';
       *{$name} = *{"get_$name"} =  sub { ... };
   }
}

sub convert_blessed {
    my $self = shift->SUPER::convert_blessed(@_);
    my $meth = $self->SUPER::get_convert_blessed ? 'add_fallback' : 'remove_fallback';
    $self->$meth('blessed', '_convert_with_to_json');
}

sub get_convert_bignum { shift->SUPER::get_allow_bignum }

sub convert_bignum {
    my $self = shift->SUPER::allow_bignum(@_);
    my $meth = $self->SUPER::get_allow_bignum ? 'add_fallback' : 'remove_fallback';
    $self->$meth('blessed', '_convert_bignum');
}

sub get_convert_as_nonblessed { shift->SUPER::get_as_nonblessed }

sub convert_as_nonblessed {
    my $self = shift->SUPER::as_nonblessed(@_);
    my $meth = $self->SUPER::get_as_nonblessed ? 'add_fallback' : 'remove_fallback';
    $self->$meth('blessed', '_convert_as_nonblessed');
}

###

{
    sub object_to_json {
        my ($self, $obj) = @_;
        my $type = ref($obj);

        if($type eq 'HASH'){
            return $self->hash_to_json($obj);
        }
        elsif($type eq 'ARRAY'){
            return $self->array_to_json($obj);
        }
        elsif ($type) { # blessed object?
            if (blessed($obj)) {

                return $self->value_to_json($obj) if ( $obj->isa('JSON::PP::Boolean') );

                my $allow_blessed = $self->get_allow_blessed;
                if ($allow_blessed) {
                    if (my ($r) = $self->_emit_fallback_to_json('blessed', $obj)) {
                        return $r;
                    }
                    else {
                        encode_error( sprintf("encountered object '%s', but no fallback "
                            . "for 'blessed' was matched", $obj )
                        );
                    }
                }
                encode_error( sprintf("encountered object '%s', but allow_blessed "
                    . "setting is not enabled", $obj)
                );
            }
            else {
                return $self->value_to_json($obj);
            }
        }
        else{
            return $self->value_to_json($obj);
        }
    }

    sub value_to_json {
        my ($self, $value) = @_;

        return 'null' if(!defined $value);

        my $type = ref($value);

        if (!$type) {
            if (_looks_like_number($value)) {
                return $value;
            }
            return $self->string_to_json($value);
        }
        elsif( blessed($value) and  $value->isa('JSON::PP::Boolean') ){
            return $$value == 1 ? 'true' : 'false';
        }
        else {
            if ((overload::StrVal($value) =~ /=(\w+)/)[0]) {
                return $self->value_to_json("$value");
            }

            if ($type eq 'SCALAR' and defined $$value) {
                return   $$value eq '1' ? 'true'
                       : $$value eq '0' ? 'false'
                       : $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ? 'null'
                       : encode_error("cannot encode reference to scalar");
            }

            if ( $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ) {
                if (my ($r) = $self->_emit_fallback_to_json('unknown', $value)) {
                    return $r;
                }
            }
            {
                if ( $type eq 'SCALAR' or $type eq 'REF' ) {
                    encode_error("cannot encode reference to scalar");
                }
                else {
                    encode_error("encountered $value, but JSON can only represent references to arrays or hashes");
                }
            }

        }
    }

    BEGIN { *encode_error = *JSON::PP::encode_error }
}

1;

=encoding utf8

=head1 NAME

JSON::PP::Monkey – JSON::PP with encoding fallbacks

=head1 SYNOPSIS

=head1 DESCRIPTION

This is an experiment with a JSON encoder that may
apply fallback conversions to non-refs, unknowns
and blessed objects.

The primary reason it has been created was to allow
dumping arbitrary Perl data into JSON – see the upcoming
L<Shell::Perl::Dumper::JSON>.

=head1 CAVEATS

Unlike in C<JSON::PP>, C<allow_bignum> only works if C<allow_blessed>
is enabled too.

Notice that the order of fallbacks is important:

    $json = JSON::PP->new->utf8->allow_blessed->allow_bignum->as_unblessed;

works as expected – bignums converted to their stringified values
and other blessed objects encoded as their underlying data structure.
But

    $json = JSON::PP->new->utf8->allow_blessed->as_unblessed->allow_bignum;

will convert all blessed objects (including bignums) into their underlying
data structures. That happens because the "as_unblessed" fallback
is a catch-all fallback, instead of being selective as the "allow_bignum" fallback.

=head1 METHODS

all of JSON::PP and plus

=head2 add_fallback

    $json = $json->add_fallback($case, $cb);
    $json = $json->add_fallback(\@case, $cb);

Add fallback conversions to be applied in the following cases

    a non-ref is found and "allow_nonref" is enabled
    an unknown is found and "allow_unknown" is enabled
    a blessed ref is found and "allow_blessed" is enabled

C<$case> should be one of C<'nonref'>, C<'unknonwn'> or C<'blessed'>.

C<$cb> is a subroutine which expects two arguments

    sub {
        my ($json, $item, $case) = (shift, shift);
        ...
    }

Fallback subroutines are evaluated in list context.

=over 4

=item *

empty list - means try next fallback

=item *

non-empty list - convert first element to JSON

=back

=head1 remove_fallback

    $json = $json->remove_fallback($case, $cb);
