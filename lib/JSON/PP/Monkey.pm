
package JSON::PP::Monkey;

use 5.10.1;
use strict;
use warnings;

use parent qw(JSON::PP);

use Carp ();
use Scalar::Util qw(blessed refaddr);

sub add_fallback {
    my ($self, $case, $cb) = @_;
    push @{$self->{fallbacks}{$case}}, $cb and return $self;
}

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

                my $convert_blessed = $self->get_convert_blessed;
                if ( $convert_blessed and $obj->can('TO_JSON') ) {
                    my $result = $obj->TO_JSON();
                    if ( defined $result and ref( $result ) ) {
                        if ( refaddr( $obj ) eq refaddr( $result ) ) {
                            encode_error( sprintf(
                                "%s::TO_JSON method returned same object as was passed instead of a new one",
                                ref $obj
                            ) );
                        }
                    }

                    return $self->object_to_json( $result );
                }

                my $bignum = $self->get_allow_bignum;
                return "$obj" if ( $bignum and _is_bignum($obj) );

                my $allow_blessed = $self->get_allow_blessed;
                if ($allow_blessed) {
                    if (my $s = $self->{fallbacks}{blessed}) {
                        for my $cb (@$s) {
                            if (my ($r) = $self->$cb($obj, 'blessed')) {

                                if ( defined $r and ref( $r ) ) {
                                    if ( refaddr( $obj ) eq refaddr( $r ) ) {
                                        encode_error( sprintf(
                                            "'blessed' fallback (%s) returned same object as was passed instead of a new one",
                                            $cb
                                        ) );
                                    }
                                }

                                return $self->object_to_json($r);
                            }
                        }
                    }
                    my $as_nonblessed = $self->get_as_nonblessed;
                    return $self->blessed_to_json($obj) if ($as_nonblessed); # will be removed.
                    return 'null';
                }
                encode_error( sprintf("encountered object '%s', but neither allow_blessed "
                    . "nor convert_blessed settings are enabled", $obj)
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

    BEGIN { *encode_error = *JSON::PP::encode_error }
}

BEGIN { *_is_bignum = *JSON::PP::_is_bignum }

1;

=encoding utf8

=head1 NAME

JSON::PP::Monkey – JSON::PP with fallbacks

=head1 SYNOPSIS

=head1 DESCRIPTION

This is an experiment with a JSON encoder that may
apply fallback conversions to non-refs, unknowns
and blessed objects.

The primary reason it has been created was to allow
dumping arbitrary Perl data into JSON – see the upcoming
L<Shell::Perl::Dumper::JSON>.

=head1 METHODS

all of JSON::PP and plus

=head2 add_fallback

    $json = $json->add_fallback($case, $cb);
    $json = $json->add_fallback(\@case, $cb);

C<$case> should be one of C<'nonref'>, C<'unknonwn'> or C<'blessed'>.

C<$cb> is a subroutine which expects two arguments

    sub {
        my ($json, $item, $case) = (shift, shift);
        ...
    }
