###################################################################

package Utils;

###################################################################

# general utility routines

sub signed
{

	my $what=shift;
	
	my $what=$what+0;
	
	return $what if $what<=0;
	
	return "+$what";

}

sub my_reverse_lines
{

	my $what=shift;
	
	my @what=split /\n/,$what;
	
	@what=reverse(@what);
	
	$what=join("\n",@what);

}

my %months=qw(1 January 2 February 3 March 4 April 5 May 6 June 7 July 8 August 9 September 10 October 11 November 12 December);
sub normalize_date
{

	my $date=shift;
	
	if($date=~/([0-9]{4})\.([0-9]+)\.([0-9]+)/)
	{
	
		my $year=$1;
		my $month=$2+0;
		my $day=$3;
		
		return "$months{$month} $day. - $year";
	
	}
	
	return $date;

}

sub tolower
{

	my $what=shift;
	
	$what=~tr/[A-Z]/[a-z]/;
	
	return $what;

}

# pad or cut string to certain length and align it

sub limit
{

	my $what=shift;
	my $limit=shift;
	my $align=shift;
	
	if($align eq 'c')
	{
		my $pad=int(($limit-length($what))/2);
		$what=(' ' x $pad).$what;
		$align='';
		
	}
	
	if(length($what)>$limit)
		{
		
			$what=substr $what,0,$limit;
		
		}
	
	sprintf('%'.$align.$limit.'s',$what);

}

# extract y and x coordinates from text index

sub split_text_index
{

	my $index=shift;
	
	$index=~/^([^\.]+)\.(.*)/;
	
	my $coords;
	
	$coords->{y}=$1;
	$coords->{x}=$2;
	
	return $coords;

}

###################################################################

package Board;

###################################################################

# global vars

%color_of_piece		=	qw(p -1 n -1 b -1 r -1 q -1 k -1 P 1 N 1 B 1 R 1 Q 1 K 1);
%type_of_piece		=	qw(p P n N b B r R q Q k K P P N N B B R R Q Q K K);
%white_of_piece		=	%type_of_piece;
%algeb_of_piece		=	qw(p p n n b b r r q q k k P p N n B b R r Q q K k);
%black_of_piece		=	%algeb_of_piece;
%inverse_of_piece	=	qw(p P P p n N N n b B B b r R R r q Q Q q k K K k);

%vector_piece=qw(Q 1 R 1 B 1);

$start_rep='rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR';

$check_test_pieces=
{
	1	=>	'KNBRQP',
	-1	=>	'knbrqp'
};

$piece_vectors=
{
	Q=>[[0,1],[0,-1],[1,0],[-1,0],[1,1],[-1,1],[1,-1],[-1,-1]],
	K=>[[0,1],[0,-1],[1,0],[-1,0],[1,1],[-1,1],[1,-1],[-1,-1]],
	R=>[[0,1],[0,-1],[1,0],[-1,0]],
	B=>[[1,1],[-1,1],[1,-1],[-1,-1]],
	N=>[[1,2],[1,-2],[-1,2],[-1,-2],[2,1],[2,-1],[-2,1],[-2,-1]]
};

# global subs

sub pos_to_ij
{

	my $pos=shift;
	
	if(($pos<0)||($pos>63))
	{
		# out of board
		
		return(NULL);
	}
	
	my $i=$pos%8;
	my $j=($pos-$i)/8;
	
	my $ij={i=>$i,j=>$j};
	
	return($ij);
	
}

sub ij_to_pos
{

	my $i=shift;
	my $j=shift;
	
	if(($i<0)||($i>7)||($j<0)||($j>7))
	{
		# out of board
		
		return(-1);
	}
	
	my $pos=$j*8+$i;
	
	return($pos);
}

sub pos_to_algeb
{
	
	my $pos=shift;
	
	my $ij=pos_to_ij($pos);
	
	if($ij)
	{
		my $algeb={file=>chr($ij->{i}+ord('a')),rank=>chr(7-$ij->{j}+ord('1'))};
		
		$algeb->{algeb}=$algeb->{file}.$algeb->{rank};
		
		return $algeb;
	}
	else
	{
		return NULL;
	}
	
}

sub algeb_to_pos
{
	
	my $algeb=shift;
	
	$algeb=~/^(.)(.)/;
	
	my $i=ord($1)-ord('a');
	my $j=7-(ord($2)-ord('1'));
	
	return(ij_to_pos($i,$j));
	
}

# object subs

sub board_rep
{
	my $self=shift;
	
	my @fen=split //,$self->{rep};
	
	my $col=0;
	for(my $i=0;$i<@fen;$i++)
	{
	
		if(($i%8)!=0){$col=1-$col;}
		if($col==0)
		{
		
			$fen[$i]=~tr/ PNBRQKpnbrqk/ pnbrqkomvtwl/;
		
		}
		else
		{
		
			$fen[$i]=~tr/ PNBRQKpnbrqk/+PNBRQKOMVTWL/;
		
		}
	
	}
		
	return join('',@fen);
}

sub report_fen
{

	my $self=shift;
	
	my $no_counts=shift;

	my $fen=join('/',map { $_=~s/( +)/length $1/ge; $_; } $self->{rep}=~/.{8}/g );
	
	$fen.=' '.($self->{turn}==1?'w':'b');
	
	$fen.=$self->{castling_rights} eq ''?' -':" $self->{castling_rights}";
	
	$fen.=' '.($self->{ep_pos} eq ''?'-':(pos_to_algeb($self->{ep_pos})->{algeb}));
	
	if($no_counts){return $fen;}
	
	$fen.=" $self->{half_move_clock}";
	
	$fen.=" $self->{full_move_number}";
	
	return $fen;
	
}

sub set_from_fen
{

	my $self=shift;
	
	my $fen=shift;
	
	my ($rep,$turn,$castling_rights,$ep_square,$half_move_clock,$full_move_number)=split / /,$fen;
	
	$rep=~s/([0-9]+)/' ' x $1/eg;
	$rep=~s/\///g;
	$turn=$turn eq 'w'?1:-1;
	$castling_rights=~s/\-//;
	$ep_square=~s/\-//;
	$half_move_clock=$half_move_clock eq ''?'0':$half_move_clock;
	$full_move_number=$full_move_number eq ''?'1':$full_move_number;
	
	my $ep_pos;
	
	if($ep_square ne '')
	{
		$ep_pos=algeb_to_pos($ep_square);
	}
	
	$self->{rep}=$rep;
	$self->{turn}=$turn;
	$self->{castling_rights}=$castling_rights;
	$self->{ep_pos}=$ep_pos;
	
}

sub reset
{

	my $self=shift;
	
	$self->{rep}=$start_rep;
	
	$self->{castling_rights}='KQkq';
	$self->{half_move_clock}=0;
	$self->{full_move_number}=1;
	$self->{ep_pos}='';
	
	$self->{turn}=1;
	
}

sub clone
{

	my $self=shift;
	
	my $clone=new Board;
	
	$clone->{rep}=$self->{rep};
	
	$clone->{castling_rights}=$self->{castling_rights};
	$clone->{half_move_clock}=$self->{half_move_clock};
	$clone->{full_move_number}=$self->{full_move_number};
	$clone->{ep_pos}=$self->{ep_pos};
	
	$clone->{turn}=$self->{turn};
	
	return $clone;
	
}

sub copy
{

	my $self=shift;
	
	my $copy=shift;
	
	$self->{rep}=$copy->{rep};
	
	$self->{castling_rights}=$copy->{castling_rights};
	$self->{half_move_clock}=$copy->{half_move_clock};
	$self->{full_move_number}=$copy->{full_move_number};
	$self->{ep_pos}=$copy->{ep_pos};
	
	$self->{turn}=$copy->{turn};
	
}

#############################################################

sub new

#############################################################

{

	my $self={
	};
	
	bless $self;
	
	$self->reset;
	
	return $self;
	
}

sub print_as_string
{

	$self=shift;
	
	my $print=join("\n",$self->{rep}=~/.{8}/g)."\n--------\n";
	$print.="half move clock: $self->{half_move_clock}, full move number: $self->{full_move_number}\n";
	$print.="e.p. square: ---$self->{ep_pos}---, castling rights: ---$self->{castling_rights}---";
	
	return($print);
	
}

sub print
{

	$self=shift;
	
	print "------------------------\n",$self->print_as_string,"\n------------------------\n";
	
}

sub get_piece_at_pos
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=substr($self->{rep},$pos,1);
	
	return($piece);
	
}

sub set_piece_at_pos
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=shift;
	
	substr($self->{rep},$pos,1,$piece);
	
}

sub legal_targets_at_pos_for_piece
{

	my $self=shift;
	
	my $pos=shift;
	
	my $piece=shift;
	
	my $ij=pos_to_ij($pos);
	
	my $i0=$ij->{i};
	my $j0=$ij->{j};
	
	my $PIECE=$type_of_piece{$piece};
	my $color=$color_of_piece{$piece};
	
	my $target_pos_list=[];
	
	if($piece eq 'p')
	{
		if($j0==1)
		{
			if(($self->get_piece_at_pos($pos+8) eq ' ')&&($self->get_piece_at_pos($pos+16) eq ' '))
			{
				push(@{$target_pos_list},{pos=>$pos+16,ep_pos=>$pos+8});
			}
		}
		
		if($self->get_piece_at_pos($pos+8) eq ' ')
		{
			if($j0==6)
			{
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'n'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'b'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'r'});
				push(@{$target_pos_list},{pos=>$pos+8,prom=>'q'});
			}
			else
			{
				push(@{$target_pos_list},{pos=>$pos+8});
			}
		}
		
		if(($i0>0)&&(($color_of_piece{$self->get_piece_at_pos($pos+7)}==1)||($self->{ep_pos} eq ($pos+7))))
		{
			push(@{$target_pos_list},{pos=>$pos+7});
		}
		
		if(($i0<7)&&(($color_of_piece{$self->get_piece_at_pos($pos+9)}==1)||($self->{ep_pos} eq ($pos+9))))
		{
			push(@{$target_pos_list},{pos=>$pos+9});
		}
	}
	elsif($piece eq 'P')
	{
		if($j0==6)
		{
			if(($self->get_piece_at_pos($pos-8) eq ' ')&&($self->get_piece_at_pos($pos-16) eq ' '))
			{
				push(@{$target_pos_list},{pos=>$pos-16,ep_pos=>$pos-8});
			}
		}
		
		if($self->get_piece_at_pos($pos-8) eq ' ')
		{
			if($j0==1)
			{
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'N'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'B'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'R'});
				push(@{$target_pos_list},{pos=>$pos-8,prom=>'Q'});
			}
			else
			{
				push(@{$target_pos_list},{pos=>$pos-8});
			}
		}
		
		if(($i0>0)&&(($color_of_piece{$self->get_piece_at_pos($pos-9)}==-1)||($self->{ep_pos} eq ($pos-9))))
		{
			push(@{$target_pos_list},{pos=>$pos-9});
		}
		
		if(($i0<7)&&(($color_of_piece{$self->get_piece_at_pos($pos-7)}==-1)||($self->{ep_pos} eq ($pos-7))))
		{
			push(@{$target_pos_list},{pos=>$pos-7});
		}
	}
	else
	{
		my $vectors=$piece_vectors->{$PIECE};
		
		foreach(@{$vectors})
		{
			my $di=$_->[0];
			my $dj=$_->[1];
			
			for(my $step=1;$step<=($vector_piece{$PIECE}?7:1);$step++)
			{
				
				my $vector_i=$i0+$step*$di;
				my $vector_j=$j0+$step*$dj;
				my $vector_pos=ij_to_pos($vector_i,$vector_j);
				if($vector_pos<0){goto vector_finished;}
				my $piece_at_vector_pos=$self->get_piece_at_pos($vector_pos);
				if($color_of_piece{$piece_at_vector_pos}==$color){goto vector_finished;}
				
				push(@{$target_pos_list},{pos=>$vector_pos});
				
				if($color_of_piece{$piece_at_vector_pos}==-$color){goto vector_finished;}
			}
			
			vector_finished:
		}
	}

	if(($piece eq 'k')&&($pos==4))
	{
		if((substr($self->{rep},5,2) eq '  ')&&($self->{castling_rights}=~/k/))
		{
			push(@{$target_pos_list},{pos=>6,castles=>'O-O'});
		}
		
		if((substr($self->{rep},1,3) eq '   ')&&($self->{castling_rights}=~/q/))
		{
			push(@{$target_pos_list},{pos=>2,castles=>'O-O-O'});
		}
	}
	
	if(($piece eq 'K')&&($pos==60))
	{
		if((substr($self->{rep},61,2) eq '  ')&&($self->{castling_rights}=~/K/))
		{
			push(@{$target_pos_list},{pos=>62,castles=>'O-O'});
		}
		
		if((substr($self->{rep},57,3) eq '   ')&&($self->{castling_rights}=~/Q/))
		{
			push(@{$target_pos_list},{pos=>58,castles=>'O-O-O'});
		}
	}
	
	return($target_pos_list);
	
}

sub legal_moves
{

	my $self=shift;
	
	my $limit_to=shift;
	
	my $legal_moves=[];
	
	for(my $pos=0;$pos<64;$pos++)
	{
		my $piece=$self->get_piece_at_pos($pos);
		
		my $go=1;
		
		if($limit_to ne '')
		{
			if($piece ne $limit_to)
			{
				$go=0;
			}
		}
		
		if( ($color_of_piece{$piece}==$self->{turn}) && $go )
		{
			my @target_pos_list=@{$self->legal_targets_at_pos_for_piece($pos,$piece)};
			
			my $orig_algeb_data=pos_to_algeb($pos);
			
			my $orig_algeb=$orig_algeb_data->{algeb};
			my $orig_algeb_file=$orig_algeb_data->{file};
			my $orig_algeb_rank=$orig_algeb_data->{rank};
		
			foreach(@target_pos_list)
			{
				my $target_pos=$_;
				my $dest_pos=$target_pos->{pos};
				
				my $dest_piece=$self->get_piece_at_pos($dest_pos);
				
				my $dest_algeb=pos_to_algeb($dest_pos)->{algeb};
				
				my $prom=$target_pos->{prom};
				my $prom_type=$type_of_piece{$prom};
				my $prom_algeb=$algeb_of_piece{$prom};
				
				my $castles=$target_pos->{castles};
				
				my $capture;
				my $ep=( ($dest_pos eq $self->{ep_pos}) && ($type_of_piece{$piece} eq 'P') );
				
				if( ($dest_piece ne ' ') || ($ep) )
				{
					$capture=1;
				}
				
				my $pgn_piece;
				my $piece_type=$type_of_piece{$piece};
				
				$orig_algeb=~/^(.)(.)/;
				
				my $orig_file=$1;
				my $orig_rank=$2;
				
				if($piece_type eq 'P')
				{
					if($capture)
					{
						$pgn_piece=$orig_file;
					}
				}
				else
				{
					$pgn_piece=$piece_type;
				}
				
				my $pgn_takes=$capture?'x':'';
				
				my $pgn_algeb=$dest_algeb;
				
				my $pgn_ep=$ep?' e.p.':'';
				
				my $pgn_prom=$prom eq ''?'':"=$prom_type";
				
				my $pgn_move="$pgn_piece$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_rank_move="$pgn_piece$orig_algeb_rank$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_file_move="$pgn_piece$orig_algeb_file$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_full_move="$pgn_piece$orig_algeb$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				
				if($castles ne ''){$pgn_move=$castles;}
				
				push(@{$legal_moves},
				{
					orig_algeb=>$orig_algeb,
					orig_pos=>$pos,
					dest_algeb=>$dest_algeb,
					dest_pos=>$dest_pos,
					ep_pos=>$target_pos->{ep_pos},
					prom=>$prom,
					castles=>$castles,
					prom_algeb=>$prom_algeb,
					full_algeb=>"$orig_algeb$dest_algeb$prom_algeb",
					capture=>$capture,
					ep=>$ep,
					pgn_piece=>$pgn_piece,
					pgn_takes=>$pgn_takes,
					pgn_algeb=>$pgn_algeb,
					pgn_ep=>$pgn_ep,
					pgn_prom=>$pgn_prom,
					pgn_move=>$pgn_move,
					pgn_file_move=>$pgn_file_move,
					pgn_rank_move=>$pgn_rank_move,
					pgn_full_move=>$pgn_full_move,
					piece=>$piece
				}
				);
			}
		}
	}
	
	return($legal_moves);
	
}

sub legal_algeb_moves
{

	my $self=shift;
	
	my $limit_to=shift;
	
	my $legal_moves=$self->legal_moves($limit_to);
	
	my $legal_algeb_moves={};
	
	my $pgn_moves={};
	
	foreach(@{$legal_moves})
	{
		my $move=$_;
		
		my $clone=$self->clone;
		
		if($clone->make_move_legal({legal=>1,move=>$move}))
		{
			
			if(!$self->{dont_check_pgn})
			{
			
				my $check;
				my $mate;
				my $exploded;
				my $stalemate;
				
				if($clone->is_in_check(-$self->{turn}))
				{
					$check=1;
				}
				
				if($clone->is_exploded(-$self->{turn}))
				{
					$exploded=1;
				}
				
				if(!$exploded)
				{
				
					my $any_legal;
				
					if(0)
					{
						my @opp_moves=@{$clone->legal_moves};
						
						foreach(@opp_moves)
						{
							my $lmove=$_;
							my $clone2=$clone->clone;
							if($clone2->make_move_legal({legal=>1,move=>$lmove}))
								{
									$any_legal=1;goto legality_check_done;
								}
						}
						
						legality_check_done:
					
					}
					
					$any_legal=$clone->has_legal;
					
					if(!$any_legal)
					{
						if($check)
						{
							$mate=1;
						}
						else
						{
							$stalemate=1;
						}
					}
					
					if($check&&!$mate)
					{
						$move->{pgn_check}='+';
					}
					elsif($mate)
					{
						$move->{pgn_check}='#';
					}
					elsif($stalemate)
					{
						$move->{pgn_check}='=';
					}
					
				}
				else
				{
					$move->{pgn_check}='';
				}
				
				$move->{pgn_move}.=$move->{pgn_check};
				$move->{pgn_file_move}.=$move->{pgn_check};
				$move->{pgn_rank_move}.=$move->{pgn_check};
				
			}
			
			$legal_algeb_moves->{$move->{full_algeb}}=
			{
				legal=>1,
				move=>$move
			};
			
			push(@{$pgn_moves->{$move->{pgn_move}}},$move);
		
		}
	}
	
	if(!$self->{dont_check_pgn})
	{
	
		foreach(keys(%{$pgn_moves}))
		{
			my $pgn_move=$_;
			my @pgn_moves=@{$pgn_moves->{$pgn_move}};
			
			if(@pgn_moves>1)
			{
			
				my $file_ok=1;
				my $file_hash={};
				my $rank_ok=1;
				my $rank_hash={};
				
				foreach(@pgn_moves)
				{
					my $full_algeb=$_->{full_algeb};
					my $move=$legal_algeb_moves->{$full_algeb}->{move};
					
					$file_hash->{$move->{pgn_file_move}}++;
					if($file_hash->{$move->{pgn_file_move}}>1)
					{
						$file_ok=0;
					}
					$rank_hash->{$move->{pgn_rank_move}}++;
					if($rank_hash->{$move->{pgn_rank_move}}>1)
					{
						$rank_ok=0;
					}
				}
				
				foreach(@pgn_moves)
				{
					my $full_algeb=$_->{full_algeb};
					my $move=$legal_algeb_moves->{$full_algeb}->{move};
					
					if($file_ok)
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_file_move};
					}
					elsif($rank_ok)
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_rank_move};
					}
					else
					{
						$legal_algeb_moves->{$full_algeb}->{move}->{pgn_move}=$move->{pgn_full_move};
					}
				}
				
			}
		}
		
	}
	
	return($legal_algeb_moves);
	
}

##################################################################
##################################################################

sub has_legal
{

	my $self=shift;
	
	my $legal_moves=[];
	
	for(my $pos=0;$pos<64;$pos++)
	{
		my $piece=$self->get_piece_at_pos($pos);
		
		if($color_of_piece{$piece}==$self->{turn})
		{
			my @target_pos_list=@{$self->legal_targets_at_pos_for_piece($pos,$piece)};
			
			my $orig_algeb_data=pos_to_algeb($pos);
			
			my $orig_algeb=$orig_algeb_data->{algeb};
			my $orig_algeb_file=$orig_algeb_data->{file};
			my $orig_algeb_rank=$orig_algeb_data->{rank};
		
			foreach(@target_pos_list)
			{
				my $target_pos=$_;
				my $dest_pos=$target_pos->{pos};
				
				my $dest_piece=$self->get_piece_at_pos($dest_pos);
				
				my $dest_algeb=pos_to_algeb($dest_pos)->{algeb};
				
				my $prom=$target_pos->{prom};
				my $prom_type=$type_of_piece{$prom};
				my $prom_algeb=$algeb_of_piece{$prom};
				
				my $castles=$target_pos->{castles};
				
				my $capture;
				my $ep=( ($dest_pos eq $self->{ep_pos}) && ($type_of_piece{$piece} eq 'P') );
				
				if( ($dest_piece ne ' ') || ($ep) )
				{
					$capture=1;
				}
				
				my $pgn_piece;
				my $piece_type=$type_of_piece{$piece};
				
				$orig_algeb=~/^(.)(.)/;
				
				my $orig_file=$1;
				my $orig_rank=$2;
				
				if($piece_type eq 'P')
				{
					if($capture)
					{
						$pgn_piece=$orig_file;
					}
				}
				else
				{
					$pgn_piece=$piece_type;
				}
				
				my $pgn_takes=$capture?'x':'';
				
				my $pgn_algeb=$dest_algeb;
				
				my $pgn_ep=$ep?' e.p.':'';
				
				my $pgn_prom=$prom eq ''?'':"=$prom_type";
				
				my $pgn_move="$pgn_piece$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_rank_move="$pgn_piece$orig_algeb_rank$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_file_move="$pgn_piece$orig_algeb_file$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				my $pgn_full_move="$pgn_piece$orig_algeb$pgn_takes$pgn_algeb$pgn_ep$pgn_prom";
				
				if($castles ne ''){$pgn_move=$castles;}
				
				my $legal_move=
				{
					orig_algeb=>$orig_algeb,
					orig_pos=>$pos,
					dest_algeb=>$dest_algeb,
					dest_pos=>$dest_pos,
					ep_pos=>$target_pos->{ep_pos},
					prom=>$prom,
					castles=>$castles,
					prom_algeb=>$prom_algeb,
					full_algeb=>"$orig_algeb$dest_algeb$prom_algeb",
					capture=>$capture,
					ep=>$ep,
					pgn_piece=>$pgn_piece,
					pgn_takes=>$pgn_takes,
					pgn_algeb=>$pgn_algeb,
					pgn_ep=>$pgn_ep,
					pgn_prom=>$pgn_prom,
					pgn_move=>$pgn_move,
					pgn_file_move=>$pgn_file_move,
					pgn_rank_move=>$pgn_rank_move,
					pgn_full_move=>$pgn_full_move,
					piece=>$piece
				};
				
				my $clone=$self->clone;
				
				if($clone->make_move_legal({legal=>1,move=>$legal_move}))
				{
					$self->{has_legal_algeb}=$legal_move->{full_algeb};
					return 1;
				}
				
			}
		}
	}
	
	return 0;
	
}

##################################################################
##################################################################


sub make_move
{

	my $self=shift;
	my $move=shift;
	
	my $orig_pos=$move->{orig_pos};
	my $dest_pos=$move->{dest_pos};
	my $orig_piece=$move->{piece};
	my $dest_piece=$self->get_piece_at_pos($dest_pos);
	
	$self->set_piece_at_pos($orig_pos,' ');
	
	my $isep=($dest_pos eq $self->{ep_pos});
	
	if(($dest_piece eq ' ')&&(!$isep))
	{
		if($move->{prom} ne '')
		{
			$self->set_piece_at_pos($dest_pos,$move->{prom});
		}
		else
		{
			$self->set_piece_at_pos($dest_pos,$orig_piece);
		}
		
	}
	else
	{
	
		$self->set_piece_at_pos($dest_pos,' ');
		
		if($isep)
		{
			if($orig_piece eq 'p')
			{
				$self->set_piece_at_pos($dest_pos-8,' ');
			}
			else
			{
				$self->set_piece_at_pos($dest_pos+8,' ');
			}
		}
	
		my $ij=pos_to_ij($dest_pos);
		for(my $di=-1;$di<=1;$di++)
		{
			for(my $dj=-1;$dj<=1;$dj++)
			{
				my $dpos=ij_to_pos($ij->{i}+$di,$ij->{j}+$dj);
				if($dpos>=0)
				{
					my $dpiece=$self->get_piece_at_pos($dpos);
					if($type_of_piece{$dpiece} ne 'P')
					{
						$self->set_piece_at_pos($dpos,' ');
					}
				}
			}
		}
	}
	
	$self->{ep_pos}=$move->{ep_pos};
	
	$self->{turn}=-$self->{turn};
	
	if($orig_piece eq 'k')
		{
			$self->{castling_rights}=~s/[qk]//g;
			
			if($move->{castles} eq 'O-O')
			{
				$self->set_piece_at_pos(7,' ');
				$self->set_piece_at_pos(5,'r');
			}
			
			if($move->{castles} eq 'O-O-O')
			{
				$self->set_piece_at_pos(0,' ');
				$self->set_piece_at_pos(3,'r');
			}
		}
		
	if($orig_piece eq 'r')
		{
			if($orig_pos==0)
			{
				$self->{castling_rights}=~s/[q]//g;
			}
			
			if($orig_pos==7)
			{
				$self->{castling_rights}=~s/[k]//g;
			}
		}
		
	if($orig_piece eq 'K')
		{
			$self->{castling_rights}=~s/[QK]//g;
			
			if($move->{castles} eq 'O-O')
			{
				$self->set_piece_at_pos(63,' ');
				$self->set_piece_at_pos(61,'R');
			}
			
			if($move->{castles} eq 'O-O-O')
			{
				$self->set_piece_at_pos(56,' ');
				$self->set_piece_at_pos(59,'R');
			}
		}
		
	if($orig_piece eq 'R')
		{
			if($orig_pos==56)
			{
				$self->{castling_rights}=~s/[Q]//g;
			}
			
			if($orig_pos==63)
			{
				$self->{castling_rights}=~s/[K]//g;
			}
		}
	
}

sub is_in_check
{	

	my $self=shift;
	
	my $color=shift;
	
	my $pos_king=shift;
	
	my @check_test_pieces=split //,$check_test_pieces->{$color};
	
	my $king=shift(@check_test_pieces);
	
	$pos_king=$pos_king ne ''?$pos_king:index($self->{rep},$king);
	
	foreach(@check_test_pieces)
	{
		
		my $test_piece=$_;
		
		my $inv_test_piece=$inverse_of_piece{$test_piece};
		
		my @legal_moves=@{$self->legal_targets_at_pos_for_piece($pos_king,$test_piece)};
		
		foreach(@legal_moves)
		{
			my $legal_move=$_;
			my $dest_pos=$legal_move->{pos};
			my $dest_piece=$self->get_piece_at_pos($dest_pos);
			if($dest_piece eq $inv_test_piece){goto check;}
		}
		
	}
	
	return 0;
	
	check:
	
	return 1;
	
}

sub is_exploded
{

	my $self=shift;
	
	my $turn=shift;
	
	my $pos_king=index($self->{rep},$turn==1?'K':'k');
	
	return($pos_king<0);
	
}

sub make_move_legal
{

	my $self=shift;
	
	my $legal_move=shift;
	
	if($legal_move->{legal})
	{
		
		my $move=$legal_move->{move};
	
		if
			(
				(
					($move->{castles} eq 'O-O')
					&&
					( 
						( ($move->{piece} eq 'k') && ( ($self->is_in_check(-1,4)) || ($self->is_in_check(-1,5)) ) )
						||
						( ($move->{piece} eq 'K') && ( ($self->is_in_check(1,60)) || ($self->is_in_check(1,61)) ) )
					)
				)
				||
				(
					($move->{castles} eq 'O-O-O')
					&&
					( 
						( ($move->{piece} eq 'k') && ( ($self->is_in_check(-1,4)) || ($self->is_in_check(-1,3)) ) )
						||
						( ($move->{piece} eq 'K') && ( ($self->is_in_check(1,60)) || ($self->is_in_check(1,59)) ) )
					)
				)
			)
			
			{
				return(0);
			}
	
		my $clone=$self->clone;
		
		$clone->make_move($move);
		
		if(
		($clone->is_in_check($self->{turn}) && (!($clone->is_exploded(-$self->{turn}))) )
		||
		($clone->is_exploded($self->{turn}))
		)
		{
			return(0);
		}
		else
		{
			$self->copy($clone);
		}
	}
	else
	{
		return(0);
	}
	
	return(1);
	
}

sub make_algeb_move
{

	my $self=shift;
	
	my $algeb=shift;
	
	my $legal_algeb_moves=$self->legal_algeb_moves;
	
	my $legal_move=$legal_algeb_moves->{$algeb};
	
	return($self->make_move_legal($legal_move));
	
}

###################################################################

package Game;

###################################################################

use Tk;
require Tk::Dialog;

use Time::HiRes qw(gettimeofday tv_interval);
use Cwd;

use Data::Dumper;

use Storable qw(freeze thaw);

use JSON;

use Win32::Clipboard;my $CLIP=Win32::Clipboard();

############################################

# global vars

my $book_txt='book_old.txt';
my $dump_txt='dump_old.txt';

my $piece_size=40;
my $margin=10;

my $square_size=55;
my $canvas_size=$square_size*8+$margin*2;

my @minimax_lines;

my %draginfo;

my %button_pack_options=qw(-padx 5);

my %pgn_text_attr=(-foreground=>'#007f00',-background=>'#efefef',-font=>[-family=>'Courier',-size=>14,-weight=>'bold']);
my %pgn_header_text_attr=(-height=>1,-width=>14,-foreground=>'#007f00',-background=>'#efefef',-font=>[-family=>'Courier',-size=>10,-weight=>'bold']);

my @comments=qw(!! ! !? - ?! ? ??);
my $comment_line=join(' ',map { sprintf "%-2s",$_ } @comments);

my $comments={
	'!!'=>{
		name=>'exex',
		value=>5,
		tags=>{-foreground=>'#00ff00'}
	},
	'!'=>{
		name=>'ex',
		value=>3,
		tags=>{-foreground=>'#0000ff'}
	},
	'!?'=>{
		name=>'exqu',
		value=>1,
		tags=>{-foreground=>'#007f7f'}
	},
	'-'=>{
		name=>'neut',
		value=>0,
		tags=>{-foreground=>'#000000'}
	},
	'?!'=>{
		name=>'quex',
		value=>-1,
		tags=>{-foreground=>'#7f7f00'}
	},
	'?'=>{
		name=>'qu',
		value=>-3,
		tags=>{-foreground=>'#7f007f'}
	},
	'??'=>{
		name=>'ququ',
		value=>-5,
		tags=>{-foreground=>'#ff0000'}
	},
	'...'=>{
		name=>'un',
		value=>-100
	}
};

############################################

# engine

############################################

use IPC::Open2;
use threads;
use threads::shared;

use Thread::Queue;

my $q=new Thread::Queue;
my $r=new Thread::Queue;

my $engine_status=new Thread::Queue;
my $engine_action=new Thread::Queue;

############################################

my $pid = open2(\*CHLD_OUT, \*CHLD_IN, '../pulsar.exe');

my $new_command="new\nvariant atomic\n";

my $issue_command_thread=threads->create(sub {

		while(1)
		{
		
			my $command=$q->dequeue;
			
			print CHLD_IN $command;
			flush CHLD_IN;
			
		}
		
	})->detach;
	
my $read_output_thread=threads->create(sub {

		do
		
			{
			
				my $chunk=<CHLD_OUT>;
				
				my @nocontrols = grep { ( ord ( $_ ) >= 32 ) || ( $_ eq "\n" ) } split //,$chunk;
				
				$chunk=join('',@nocontrols);
				
				#print "$chunk";
				
				if($chunk=~/received move/)
				{
					$engine_status->enqueue(1);
				}
				
				if($chunk=~/finished move/)
				{
					$engine_status->enqueue(0);
				}
				
				if($chunk=~/(^[0-9\-]+)\s+([0-9\-]+)\s+[0-9\-]+\s+[0-9\-]+\s+([a-z].*)/)
					{
						my $depth=$1;
						my $eval=$2;if($eval>0){$eval='+'.$eval;}
						my $line=$3;
						my $engine_line=sprintf("%2d %5s   $line",$depth,$eval);
						
						$r->enqueue($engine_line);
					}
			
			}
			
		while(1);
			
		
	})->detach;
	

$q->enqueue("xboard\nvariant atomic\nanalyze\npost\n");
	
############################################

sub del_engine_arrow
{
	my $self=shift;
	
	if($self->{engine_arrow} ne '')
		{
			$self->{canvas}->delete($self->{engine_arrow});
		}
}

my $total_built;
sub get_legal_algeb_moves
{
	my $self=shift;
	
	my $current_pos=shift;
	
	my @legal_algeb_moves;
	
	if($self->{node_book}->{$current_pos}->{legal_algeb_moves} eq '')
	{
	
		$total_built++;
		print "$total_built: $current_pos\n-------------\n";
		
		$self->{dont_check_pgn}=1;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves;
		
		@legal_algeb_moves=sort keys(%{$legal_algeb_moves});
		
		@{$self->{node_book}->{$current_pos}->{legal_algeb_moves}}=@legal_algeb_moves;
	}
	else
	{
		@legal_algeb_moves=@{$self->{node_book}->{$current_pos}->{legal_algeb_moves}};
	}
	
	return @legal_algeb_moves;
}

sub select_node
{
	my $self=shift;
	
	my $depth=shift;
	
	if($depth>20)
	{	
		return 0;
	}
	
	my $full_algeb;

	my $current_pos=$self->{board}->report_fen(1);
	
	my @legal_algeb_moves=$self->get_legal_algeb_moves($current_pos);
	
	if(@legal_algeb_moves==0)
	{
		return 0;
	}

	foreach(@legal_algeb_moves)
	{
	
		$full_algeb=$_;
		
		if($self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval} eq '')
		{
		
			$self->{analyzed_move}=$full_algeb;
		
			$self->make_move($full_algeb);
			
			$self->move_made;
			
			$self->{analyzing}=1;
			
			$self->{analysis_pos}=$current_pos;
			
			$self->analyze_pos;
			
			return 1;
			
		}
		
	}
	
	@legal_algeb_moves=sort
	{
	
		$self->{board}->{turn}==1
		?
		$self->{after_move_book}->{$current_pos}->{$a}->{eval}<=>$self->{after_move_book}->{$current_pos}->{$b}->{eval}
		:
		$self->{after_move_book}->{$current_pos}->{$b}->{eval}<=>$self->{after_move_book}->{$current_pos}->{$a}->{eval}
		
		
	}@legal_algeb_moves;
	
	while(@legal_algeb_moves)
	{
		my $selected_full_algeb=pop(@legal_algeb_moves);
		
		if(rand(100)>40)
		{
			$self->make_move($selected_full_algeb);
	
			return $self->select_node($depth+1);
		}
	}
	
	return 0;
	
}

sub minimax_out
{
	my $self=shift;
	
	my $forced=shift;
	
	return if ( ( $self->{analysis_mode} && (!$forced) ) || ( $self->{minimax_in_progress} ) );
	
	$self->{minimax_in_progress}=1;
	
	$self->{engine_minimax_button}->configure(-text=>'Minimax in progess!');$self->{engine_minimax_button}->update;
	
	$self->minimax(0);
	
	$self->{engine_minimax_button}->configure(-text=>'Minimax');$self->{engine_minimax_button}->update;
	
	$self->move_made;
	
	$self->{minimax_in_progress}=0;

}

sub minimax
{
	
	my $self=shift;
	
	my $depth=shift;
	
	my $line=shift;

	my $current_pos=$self->{board}->report_fen(1);
	
	if(!$self->{board}->has_legal)
	{
		return 'unknown';
	}
	else
	{
	
		if($self->{after_move_book}->{$current_pos}->{$self->{board}->{has_legal_algeb}}->{orig_eval} eq '')
		{	
			delete($self->{after_move_book}->{$current_pos});
			return 'unknown';
		}
		
	}
	
	my @legal_algeb_moves=$self->get_legal_algeb_moves($current_pos);
	
	my $best=$self->{board}->{turn}==1?-10000:10000;
	
	foreach(@legal_algeb_moves)
	{
	
		my $full_algeb=$_;
		
		my $eval=$self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval};
		
		if($eval eq '')
		{
			delete($self->{after_move_book}->{$current_pos});
			return 'unknown';
		}
		
		if($depth<20)
		{
			
			my $fen_after=$self->{after_move_book}->{$current_pos}->{$full_algeb}->{fen_after};
			
			my $returned_eval='unknown';
			
			my $clone=$self->{board}->clone;
			
			if($fen_after ne '')
			{
			
				$self->{board}->set_from_fen($fen_after);
				
				$returned_eval=$self->minimax($depth+1,"$line $full_algeb");
			
			}
			
			$self->{board}->copy($clone);
			
			if($returned_eval ne 'unknown')
			{
				$eval=$returned_eval;
			}
			
		}
		
		$self->{after_move_book}->{$current_pos}->{$full_algeb}->{eval}=$eval;
		
		$best=
			$self->{board}->{turn}==1
			?
			$eval>$best?$eval:$best
			:
			$eval<$best?$eval:$best
		;
		
	}
	
	push(@minimax_lines,"$line: $best") if ($depth==2);
	
	return $best;
	
}

sub determine_node
{

	my $self=shift;
	
	$self->{analysis_mode}=0;$self->del_move;$self->{analysis_mode}=1;
	
	my $legal_algeb_moves=$self->{board}->legal_algeb_moves;

	my $full_algeb;

	my $current_pos=$self->{board}->report_fen(1);

	foreach(sort keys(%{$legal_algeb_moves}))
	{
	
		$full_algeb=$_;
		
		if($self->{after_move_book}->{$current_pos}->{$full_algeb}->{orig_eval} eq '')
		{
		
			$self->{analyzed_move}=$full_algeb;
		
			$self->make_move($full_algeb);
			
			$self->move_made;
			
			$self->{analyzing}=1;
			
			$self->{analysis_pos}=$current_pos;
			
			$self->analyze_pos;
			
			return;
			
		}
		
	}

	$self->{analysis_mode}=0;$self->reset_game;$self->{board}->set_from_fen($self->{root});$self->move_made;$self->{analysis_mode}=1;
	
	$self->{board_frame}->update;
	
	@minimax_lines=();
	
	if( ( $self->{do_minimax} % $self->{settings}->{minimax_after} ) == 0 )
	{
		$self->minimax_out(1);
	}
	
	$self->{do_minimax}++;
	
	@minimax_lines=sort
	{
		$a=~/\: (.*)/;my $av=$1+0;
		$b=~/\: (.*)/;my $bv=$1+0;
		
		return $bv <=> $av;
	}@minimax_lines;
	
	print join('',map { $_=~/\:(.*)/;abs($1)<500?"$_\n":''; } @minimax_lines)."-------------\n";
	
	do
	{
		$self->{analysis_mode}=0;$self->reset_game;$self->{board}->set_from_fen($self->{root});$self->{analysis_mode}=1;
	}
	while(!$self->select_node(0));

}

sub update_engine
{
	my $self=shift;
	
	######################################################

	my $action;
	
	while($engine_action->pending)
	{
		$action=$engine_action->dequeue;
	}
	
	######################################################
	
	my $old_engine_status=$self->{engine_running};
	
	while($engine_status->pending)
	{
		$self->{engine_running}=$engine_status->dequeue;
	}
	
	if( ($old_engine_status) && (!$self->{engine_running}) && (!$self->{analysis_mode}) )
	{
		# normal engine analysis stopped
		
		$self->{analyze_pos_started}=0;
	}
	
	######################################################
	
	my $old_analysis_mode=$self->{analysis_mode};
	
	if(!($self->{analysis_mode}))
	{
	
		if($action eq 'analyze_pos')
		{
		
			if($self->{engine_on})
			{
				$self->analyze_pos;
				
				$self->{analyze_pos_started}=1;
			}
			
		}
			
		if($action eq 'stop')
		{
			$q->enqueue("$new_command");
		}
		
		if(!$self->{analyze_pos_started})
		{
		
			if($action eq 'change_analysis')
			{
			
				my $root=$self->{board}->report_fen(1);
				
				$self->{root}=$root;
			
				#$self->reset_game;$self->move_made;
			
				$self->{analysis_mode}=1;
				
				$self->{do_minimax}=1;
			
			}
		
		}
	
	}
	else
	{
	
		if($action eq 'change_analysis')
		{
			$self->{analysis_mode}=0;
			
			$self->{analyzing}=0;
			
			$q->enqueue("$new_command");
			
			$self->reset_game;$self->{board}->set_from_fen($self->{root});$self->move_made;
		}
		
	}
	
	######################################################
	
	if( ($self->{engine_running}!=$old_engine_status) || ($self->{analysis_mode}!=$old_analysis_mode) )
	{
		$self->{engine_frame}->configure(-background=>
			$self->{analysis_mode}?'#afafff':
			$self->{engine_running}?'#afffaf':$self->{engine_frame_inactive_background}
		);
		
		$self->{engine_analysis_button}->configure(-text=>$self->{analysis_mode}?'* Stop analysis *':'Start analysis');
		
		$self->{engine_analysis_button}->configure(-state=>$self->{analyze_pos_started}?'disabled':'normal');
		$self->{engine_go_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		$self->{engine_stop_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		$self->{engine_make_button}->configure(-state=>$self->{analysis_mode}?'disabled':'normal');
		
	}
	
	while($r->pending)
	{
	
		my $depth;
		my $eval;
	
		my $engine_line=$r->dequeue;
		
		$engine_line=~s/^\s+//;
		my @fields=split /\s+/,$engine_line;
		
		$depth=$fields[0]+0;
		$eval=$fields[1]+0;
		
		$self->{engine_line}->delete('1.0','end');
		$self->{engine_line}->insert('end',$engine_line);
		
		if(!$self->{analysis_mode})
		{
			
			my $tag='good';
			if($eval<200)
			{
				if($eval>-200)
				{
					$tag='neutral';
				}
				else
				{
					$tag='bad';
				}
			}
			
			$self->{engine_line}->tagAdd($tag,"1.0","end");
			
			$fields[2]=~/(.)(.)(.)(.)/;
			
			my $x1=ord($1)-ord('a');
			my $y1=7-(ord($2)-ord('1'));
			my $x2=ord($3)-ord('a');
			my $y2=7-(ord($4)-ord('1'));
			
			my $ij_1={i=>$x1,j=>$y1};
			my $screen_ij_1=$self->ij_to_screen_ij($ij_1);
			
			my $ij_2={i=>$x2,j=>$y2};
			my $screen_ij_2=$self->ij_to_screen_ij($ij_2);
			
			$self->del_engine_arrow;
			
			if($self->{engine_on})
			{
				$self->{engine_arrow}=$self->{canvas}->createLine(
				($screen_ij_1->{i}+0.5)*$square_size+$margin,($screen_ij_1->{j}+0.5)*$square_size+$margin,
				($screen_ij_2->{i}+0.5)*$square_size+$margin,($screen_ij_2->{j}+0.5)*$square_size+$margin,
				-arrow=>'last',
				-fill=>'#ff0000',
				-width=>2
				);
			}
			
			$self->{engine_move}=$fields[2];
			
		}
		else
		{
			
			# process engine in analysis mode
			
			$self->{engine_line}->tagRemove('good',"1.0","end");
			$self->{engine_line}->tagRemove('neutral',"1.0","end");
			$self->{engine_line}->tagRemove('bad',"1.0","end");
			
			if($depth>=5)
			{
			
				my $eval=$self->{board}->{turn}==1?$eval:-$eval;
				
				$self->{after_move_book}->{$self->{analysis_pos}}->{$self->{analyzed_move}}->{orig_eval}=$eval;
				
				$self->{after_move_book}->{$self->{analysis_pos}}->{$self->{analyzed_move}}->{eval}=$eval;
				
				$q->enqueue("$new_command");
				
				$self->{analyzing}=0;
			}
			
		}
		
	}
	
	if( (!$self->{engine_status}) && ($self->{analysis_mode}) && (!$self->{analyzing}) )
	{
	
		$self->determine_node;
	
	}
	
	if(!$self->{engine_on}){$self->del_engine_arrow;}
	
	$self->{book_text}->tagRemove('sel','1.0','end');
	
	$self->{mw}->after(500,[\&update_engine,$self]);
}

sub save_engine_status
{
	my $self=shift;
	$self->{engine_on_old}=$self->{engine_on};
}

sub restore_engine_status
{
	my $self=shift;
	$self->{engine_on}=$self->{engine_on_old};
}

sub analyze_pos
{
	$self=shift;
	
	my $line=join( "\n", map { $_->{full_algeb} } @{$self->{moves}} );
	
	$q->enqueue("$new_command$line\n");
}

sub change_analysis
{
	my $self=shift;
	
	$engine_action->enqueue('change_analysis');
}

sub go
{
	my $self=shift;
	
	$self->{engine_on}=1;
	
	$engine_action->enqueue('analyze_pos');
}

sub stop
{
	my $self=shift;
	
	$self->{engine_on}=0;
	
	$engine_action->enqueue('stop');
}

sub make_engine_move
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->make_move($self->{engine_move});
	
	$self->move_made;
}

#####################################################################################

sub reset
{
	my $self=shift;
	
	$self->{board}=new Board;
	
	$self->{flip}=0;
	
	$self->{moves}=[];
	$self->{unmoves}=[];
	
	$self->{show_legal_moves}=0;
}

sub update_book

{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{book_text}->delete('1.0','end');
	
	my $fen=$self->{board}->report_fen(1);
	
	my $y=2;
	
	my @moves=keys(%{$self->{book}->{$fen}});
	
	@moves=sort
		{
			my $comment_sort = ( $comments->{$self->{book}->{$fen}->{$b}}->{value} <=> $comments->{$self->{book}->{$fen}->{$a}}->{value} ) ;
			
			return $comment_sort if $comment_sort!=0;
			
			$self->{board}->{turn}==1
			?
			$self->{after_move_book}->{$fen}->{$b}->{eval} <=> $self->{after_move_book}->{$fen}->{$a}->{eval}
			:
			$self->{after_move_book}->{$fen}->{$a}->{eval} <=> $self->{after_move_book}->{$fen}->{$b}->{eval}
			
		}
		@moves;
	
	$self->{book_text}->insert('end',"\n");
	
	foreach(@moves)
	{
		my $full_algeb=$_;
		my $comment=$self->{book}->{$fen}->{$full_algeb};
		$self->{book_text}->insert('end',sprintf("%-6s %-6s $comment_line \n",$full_algeb,$comment));
		
		my $x=14;
		foreach(@comments)
		{
			$self->{book_text}->tagAdd($comments->{$_}->{name},"$y.$x","$y.".($x+2));
			$x+=3;
		}
		
		if($comment ne '...')
		{
			$self->{book_text}->tagAdd($comments->{$comment}->{name},"$y.0","$y.13");
		}
		
		$self->{book_text}->tagAdd('back',"$y.13","$y.36");
		
		$y++;
		
		my $fen_after=$self->{after_move_book}->{$fen}->{$full_algeb}->{fen_after};
		$self->{book_text}->insert('end',"  -> $self->{after_move_book}->{$fen}->{$full_algeb}->{visited} , eval: $self->{after_move_book}->{$fen}->{$full_algeb}->{eval} , orig eval: $self->{after_move_book}->{$fen}->{$full_algeb}->{orig_eval}\n");
		
		$self->{book_text}->tagAdd('small',"$y.0","$y.80");
		
		$y++;
	}
	
	$self->{book_text}->tagAdd('margin',"1.0","end");
	
}

sub update_legal_moves
{
	my $self=shift;
	
	$self->{legal_moves_text}->delete('1.0','end');
	
	if($self->{show_legal_moves})
		{
		
		$self->{board}->{dont_check_pgn}=0;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves;

		$self->{legal_moves_text}->delete('1.0','end');

		my @pgn_moves=map { '  '.$legal_algeb_moves->{$_}->{move}->{pgn_move} } keys(%{$legal_algeb_moves});
		@pgn_moves=sort @pgn_moves;
		$self->{legal_moves_text}->insert('end',"\n".join("\n",@pgn_moves));
		
		}
}

sub move_made
{
	my $self=shift;
	
	# make necessary updates after move
	
	$self->update_legal_moves;
	
	$self->update_book;
	
	$self->draw_board;
	
	# analyze if needed
	
	if($self->{engine})
	{
		$self->go;
	}
	else
	{
		$engine_action->enqueue('analyze_pos');
	}

}

sub load_settings
{
	my $self=shift;
	
	open(SETTINGS,"settings.txt");
	my $settings=join('',<SETTINGS>);
	close(SETTINGS);
	
	eval($settings);
	
	if(!exists($self->{settings}->{dump_depth})){$self->{settings}->{dump_depth}="15";}
	if(!exists($self->{settings}->{depth_percent})){$self->{settings}->{depth_percent}="90";}
	if(!exists($self->{settings}->{minimax_after})){$self->{settings}->{minimax_after}="20";}
	
}

sub save_settings
{
	my $self=shift;
	
	open(SETTINGS,">settings.txt");
	print SETTINGS Data::Dumper->Dump([$self->{settings}],['$self->{settings}']);
	close(SETTINGS);
}

sub reset_game
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{board}->reset;
	
	$self->{moves}=[];
	$self->{unmoves}=[];
	
	$self->stop;
	
	$self->{pgn_white}->delete('1.0','end');
	$self->{pgn_white}->insert('end',"?");
	$self->{pgn_white_elo}->delete('1.0','end');
	$self->{pgn_white_elo}->insert('end',"?");
	$self->{pgn_black}->delete('1.0','end');
	$self->{pgn_black}->insert('end',"?");
	$self->{pgn_black_elo}->delete('1.0','end');
	$self->{pgn_black_elo}->insert('end',"?");
	$self->{pgn_date}->delete('1.0','end');
	$self->{pgn_date}->insert('end',"?");
	$self->{pgn_result}->delete('1.0','end');
	$self->{pgn_result}->insert('end',"?-?");
	$self->{pgn_time_control}->delete('1.0','end');
	$self->{pgn_time_control}->insert('end',"?");
	$self->{pgn_ply_count}->delete('1.0','end');
	$self->{pgn_ply_count}->insert('end',"?");
	$self->{pgn_dir}->delete('1.0','end');
	$self->{pgn_dir}->insert('end',"default.pgn");
	
	$self->{mw}->configure(-title=>'Atomic Chess');
	
	$self->move_made;
	
	my $bck=$self->{load_1_button}->cget(-background);
	my $abck=$self->{load_1_button}->cget(-activebackground);
	for(my $j=1;$j<=5;$j++)
	{
		$self->{"save_".$j."_button"}->configure(-background=>$bck,-activebackground=>$abck);
	}
}

sub flip_board
{
	my $self=shift;
	
	$self->{flip}=!$self->{flip};
	
	$self->draw_board;
}

sub canvas_to_square
{
	my $what=shift;
	
	$what-=$margin;
	$what-= ( $what % $square_size );
	$what/=$square_size;
	
	return $what;
}

sub drag_start {

    my ($c,$self) = @_;
	
	return if $self->{analysis_mode};
	
    my $e = $c->XEvent;
    my ( $sx, $sy ) = ( $e->x, $e->y,,, );
    my ( $cx, $cy ) = ( $c->canvasx($sx), $c->canvasy($sy) );
    my $id = $c->find( 'withtag', 'current' );
    my ( $x1, $y1, $x2, $y2 ) = $c->bbox($id);
	
    $draginfo{id}     = $id;
    $draginfo{startx} = $draginfo{lastx} = $cx;
    $draginfo{starty} = $draginfo{lasty} = $cy;
	
	my $s_i=canvas_to_square($sx);
	my $s_j=canvas_to_square($sy);
	
	my $screen_ij={i=>$s_i,j=>$s_j};
	
	my $ij=$self->screen_ij_to_ij($screen_ij);
	
	my $s_col=($ij->{i}%2?!($ij->{j}%2):$ij->{j}%2);
	
	my $p=$c->itemcget($draginfo{id},-text);
	
	my $p_col=0;
	
	if($p=~/[PNBRQKpnbrqk]/){$p_col=1;}
	
	$p=~tr/+PNBRQKOMVTWL/ pnbrqkomvtwl/;
	
	$c->createText(
			$screen_ij->{i}*$square_size+$margin+$square_size/2,$screen_ij->{j}*$square_size+$square_size/2+$margin,
			-text => $s_col?'+':' ',
			-font => [ -family => 'Chess Merida', -size => $piece_size ],
			);
	
	$c->itemconfigure($draginfo{id},
		-text=>$p,
		-font => [ -family => 'Chess Merida', -size => $piece_size*1.1 ],
		-fill => $p_col?'#0000ff':'#00007f');

}

sub drag_during {
	
	my ($c) = @_;
	my $e = $c->XEvent;
	my ( $sx, $sy ) = ( $e->x, $e->y,,, );
	my ( $cx, $cy ) = ( $c->canvasx($sx), $c->canvasy($sy) );
	my ( $dx, $dy ) = ( $cx - $draginfo{lastx}, $cy - $draginfo{lasty} );
	$c->move( $draginfo{id}, $dx, $dy );
	$draginfo{lastx} = $cx;
	$draginfo{lasty} = $cy;
	
	my ( $x1, $y1, $x2, $y2 ) = $c->bbox( $draginfo{id} );
	
}

sub make_move
{
	my $self=shift;
	
	my $full_algeb=shift;
	
	$self->{board}->{dont_check_pgn}=1;
	
	my $fen_before=$self->{board}->report_fen(1);
	
	if($self->{board}->make_algeb_move($full_algeb))
	{
	
		$self->{unmoves}=[];
		
		my $fen_after=$self->{board}->report_fen(1);
		
		push(@{$self->{moves}},{full_algeb=>$full_algeb,fen_before_move=>$fen_before,fen_after_move=>$fen_after});
		
		my $comment=$self->{book}->{$fen_before}->{$full_algeb};
		
		if($comment eq '')
		{
			$self->{book}->{$fen_before}->{$full_algeb}='...';
		}
		
		my $fen_after=$self->{board}->report_fen(1);
		
		$self->{after_move_book}->{$fen_before}->{$full_algeb}->{fen_after}=$fen_after;
		
		$self->{after_move_book}->{$fen_before}->{$full_algeb}->{visited}++;
		
		return(1);
		
	}
	else
	{
		return(0);
	}
}

sub drag_end {

	shift;
	$self=shift;
	
	return if $self->{analysis_mode};
	
	my $start_i=canvas_to_square($draginfo{startx});
	my $end_i=canvas_to_square($draginfo{lastx});
	my $start_j=canvas_to_square($draginfo{starty});
	my $end_j=canvas_to_square($draginfo{lasty});
	
	my $start_screen_ij={i=>$start_i,j=>$start_j};
	my $start_ij=$self->screen_ij_to_ij($start_screen_ij);
	
	my $end_screen_ij={i=>$end_i,j=>$end_j};
	my $end_ij=$self->screen_ij_to_ij($end_screen_ij);
	
	my $diff_i=$end_i-$start_i;
	my $diff_j=$end_j-$start_j;
	
	#############################################
	# process move
	
	my $start_pos=Board::ij_to_pos($start_ij->{i},$start_ij->{j});
	my $end_pos=Board::ij_to_pos($end_ij->{i},$end_ij->{j});
	
	my $start_algeb=Board::pos_to_algeb($start_pos);
	my $end_algeb=Board::pos_to_algeb($end_pos);
	
	my $algeb=$start_algeb->{algeb}.$end_algeb->{algeb};
	
	my $orig_piece=$self->{board}->get_piece_at_pos($start_pos);
	
	my $prom_algeb;
	
	if(
		(
			($orig_piece eq 'P') && ($start_ij->{j}==1) && ($diff_i==0)
		)
		||
		(
			($orig_piece eq 'p') && ($start_ij->{j}==6) && ($diff_i==0)
		)
	)
	{
	
		$answer = $self->{mw}->Dialog(-title => 'Promote', 
			   -text => 'Select piece', 
			   -default_button => 'Queen', -buttons => [ 'Queen', 'Knight', 'Bishop','Rook'], 
			   -bitmap => 'question' )->Show( );
		
		$answer=~/^(.)/;$prom_algeb=$1;$prom_algeb=~tr/[kK]/[nN]/;
		$prom_algeb=~tr/[A-Z]/[a-z]/;
	}
	
	my $full_algeb="$algeb$prom_algeb";
	
	$self->make_move($full_algeb);
	
	$self->move_made;
	
	#############################################
	
    %draginfo = ();
}

sub del_move
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dont_show=shift;
	
	if(@{$self->{moves}}>0)
	{
		my $move=pop(@{$self->{moves}});
		push(@{$self->{unmoves}},$move);
		
		$self->{board}->set_from_fen($move->{fen_before_move});
		
		if($dont_show)
		{
		}
		else
		{
			$self->move_made;
		}
		
		return(1);
	}
	
	return(0);
}

sub undel_move
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dont_show=shift;
	
	if(@{$self->{unmoves}}>0)
	{
		my $move=pop(@{$self->{unmoves}});
		push(@{$self->{moves}},$move);
		
		$self->{board}->set_from_fen($move->{fen_after_move});
		
		if($dont_show)
		{
		}
		else
		{
			$self->move_made;
		}
		
		return(1);
	}
	
	return(0);
}

sub del_all_moves
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{engine}=0;
	$self->stop;
	
	while($self->del_move(1)){};
	
	$self->move_made;
}

sub undel_all_moves
{
	
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->{engine}=0;
	$self->stop;
	
	while($self->undel_move(1)){};
	
	$self->move_made;
}

sub copy_fen
{
	my $self=shift;
	
	my $fen=$self->{board}->report_fen;
	
	$CLIP->Set($fen);
	
}

sub show_pgn
{
	my $self=shift;
	
	my $full_pgn=shift;
	
	my $width=shift;
	
	my $show_pgn=$self->{mw}->Toplevel(-title=>'PGN');
	$show_pgn->geometry('+5+5');

	my $show_pgn_text=$show_pgn->Scrolled('Text',%pgn_text_attr,-width=>($width or 60),-height=>30,-scrollbars=>$width eq ''?'e':'se',-wrap=>$width eq ''?'word':'none')->pack;
	
	$show_pgn_text->insert('end',$full_pgn);
}

sub copy_pgn
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $save=shift;
	
	$self->{board_frame}->update;
	
	my @pgn_body;
	my $cnt=1;
	my $half_move_cnt=0;
	
	my $old=$self->{board}->clone;
	
	my $clone=new Board;
	
	my $number_of_moves=@{$self->{moves}}+0;
	
	my $t0=[gettimeofday];
	
	foreach(@{$self->{moves}})
	{
	
		my $move=$_;
		
		$half_move_cnt++;
		
		my $full_algeb=$move->{full_algeb};
		
		$full_algeb=~/^(..)/;
		my $orig_algeb=$1;
		
		$clone->set_from_fen($move->{fen_before_move});
		
		my $piece=$clone->get_piece_at_pos(Board::algeb_to_pos($orig_algeb));

		$clone->{dont_check_pgn}=0;
		my $legal_algeb_moves=$clone->legal_algeb_moves($piece);
		
		my $pgn_move=$legal_algeb_moves->{$move->{full_algeb}}->{move}->{pgn_move};
		
		my $move_number;
		if($clone->{turn}==1)
		{
			$move_number="$cnt. ";
		}
		else
		{
			$cnt++;
		}
		
		push(@pgn_body,"$move_number$pgn_move");
		
		$self->{board}->set_from_fen($move->{fen_after_move});
		
		if(0)
		{
			$self->draw_board();
			$self->{board_frame}->update;
		}
		
		$elapsed = tv_interval ($t0, [gettimeofday]);
		
		$self->report("processing move $half_move_cnt of $number_of_moves ( elapsed $elapsed )");
		
	}
	
	my $pgn_body=join(' ',@pgn_body);
	
	my $white=$self->{pgn_white}->get('1.0','end-1c');
	my $white_elo=$self->{pgn_white_elo}->get('1.0','end-1c');
	my $black=$self->{pgn_black}->get('1.0','end-1c');
	my $black_elo=$self->{pgn_black_elo}->get('1.0','end-1c');
	my $date=$self->{pgn_date}->get('1.0','end-1c');
	my $result=$self->{pgn_result}->get('1.0','end-1c');
	my $time_control=$self->{pgn_time_control}->get('1.0','end-1c');
	my $ply_count=$self->{pgn_ply_count}->get('1.0','end-1c');
	my $dir=$self->{pgn_dir}->get('1.0','end-1c');
	
	my $sep=' ';
	if($pgn_body eq ''){$sep='';}
	
	my $full_pgn=qq([White "$white"]
[WhiteElo "$white_elo"]
[Black "$black"]
[BlackElo "$black_elo"]
[Date "$date"]
[Result "$result"]
[TimeControl "$time_control"]
[PlyCount "$ply_count"]
[Variant "Atomic"]

$pgn_body$sep$result

);
	
	if($save)
	{	
		
		open(OUTF,">$self->{settings}->{pgn_dir}/$dir");
		print OUTF $full_pgn;
		close(OUTF);
		
	}
	
	if( ($self->{settings}->{show_pgn}) || (!$save) )
	{
		#$CLIP->Set($full_pgn);
		
		$self->show_pgn($full_pgn);
	}
	
	$self->{board}->copy($old);
	
}

sub import_i
{
	my $self=shift;
	
	my $i=shift;
	
	$self->reset_game;
	
	open(GAME,"../Perl2/game$i.txt");
	my $game=join('',<GAME>);
	close(GAME);
	
	my @moves=split /  /,$game;
	
	foreach(@moves)
	{
		my $move=$_;
		$move=~s/X//;
		my $prom_algeb;
		if($move=~/\*(.)/)
		{
			$prom_algeb=$1;
			$prom_algeb=~tr/[A-Z]/[a-z]/;
		}
		$move=~/^(....)/;
		my $full_algeb="$1$prom_algeb";
		
		$self->make_move($full_algeb);
	}
	
	$self->draw_board;
}

sub save_i
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $i=shift;
	
	open(GAME,">game$i.txt");
	print GAME Data::Dumper->Dump([$self->{moves}],['$self->{moves}']);
	close(GAME);
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat("game$i.txt");
	
	$self->report("game saved $size characters");
}

sub load_i
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $i=shift;
	
	$self->reset_game;
	
	open(GAME,"game$i.txt");
	my $game=join('',<GAME>);
	close(GAME);
	
	eval($game);
	
	if(@{$self->{moves}}>0)
	{
		my $last_move=pop(@{$self->{moves}});
		push(@{$self->{moves}},$last_move);
		
		$self->{board}->set_from_fen($last_move->{fen_after_move});
	}
	
	$self->move_made;
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat("game$i.txt");
	
	$self->report("game loaded $size characters");
	
	for(my $j=1;$j<=5;$j++)
	{
		$self->{"save_".$j."_button"}->configure(-background=>$i==$j?'#afffaf':'#ffafaf',-activebackground=>$i==$j?'#7fff7f':'#ff7f7f');
	}
}

sub load_book
{
	my $self=shift;
	
	open(BOOK,$book_txt);
	my $book=join('',<BOOK>);
	close(BOOK);

	eval($book);
	
	# purge book
	if(0)
	{
		foreach(keys(%{$self->{after_move_book}}))
		{
			my $fen=$_;
			foreach(keys(%{$self->{after_move_book}->{$fen}}))
			{
				my $full_algeb=$_;
				delete($self->{after_move_book}->{$fen}->{$full_algeb}->{board_after});
			}
		}
	}
}

sub save_book
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	open(BOOK,">$book_txt");
	print BOOK Data::Dumper->Dump([$self->{book},$self->{after_move_book}],['$self->{book}','$self->{after_move_book}']);
	close(BOOK);
	
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$book_size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($book_txt);

	$self->report("book saved $book_size characters");
	
}

sub book_clicked
{
	
	shift;
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $coords=Utils::split_text_index($self->{book_text}->index('current'));
	
	my $line=$self->{book_text}->get("$coords->{y}.0","$coords->{y}.5");
	
	$line=~/^([^ ]+)/;
	
	my $full_algeb=$1;
	
	if($full_algeb=~/^[a-z0-9]{4,5}/)
	{
		
		if($coords->{x}<6)
		{
			$self->make_move($full_algeb);
			
			$self->move_made;
		}
		elsif($coords->{x}>13)
		{
			my $c=int(($coords->{x}-14)/3);
			my $cc=$comments[$c];
			
			my $fen=$self->{board}->report_fen(1);
			$self->{book}->{$fen}->{$full_algeb}=$cc;
			
			$self->move_made;
		}
		
	}
}

sub dump_book
{
	my $self=shift;
	
	return if $self->{analysis_mode};
	
	$self->reset_game;
	
	open(DUMP,">$dump_txt");
	$self->dump_recursive(0,'');
	close(DUMP);
	
	$self->reset_game;
	
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($dump_txt);
	
	$self->report("book dumped $size characters");
	
	open(DUMP,$dump_txt);
	my $dump=join('',<DUMP>);
	$self->show_pgn($dump,120);
	close(DUMP);
	
}

sub dump_recursive
{
	my $self=shift;
	
	my $level=shift;
	
	my $line=shift;
	
	if($level>$self->{settings}->{dump_depth})
	{
		print DUMP "$line\n";
		return;
	}
	
	my $fen=$self->{board}->report_fen(1);
	
	my @keys=keys(%{$self->{after_move_book}->{$fen}});
	
	if(@keys>0)
	{
		foreach(@keys)
		{
			my $full_algeb=$_;
			
			my $fen_after=$self->{after_move_book}->{$fen}->{$full_algeb}->{fen_after};
			
			$self->{board}->set_from_fen($fen_after);
			
			my $comment=$self->{book}->{$fen}->{$full_algeb};if($comment eq '...'){$comment='';}else{$comment=" $comment";}
			
			$self->dump_recursive($level+1,"$line $full_algeb$comment");
		}
	}
	else
	{
		print DUMP "$line\n";
	}
	
}

sub load_pgn
{

	my $self=shift;
	
	return if $self->{analysis_mode};
	
	my $dont_create=shift;
	
	my $pgn_dir=$self->{settings}->{pgn_dir};
	
	if(!$dont_create)
	{
		$self->{show_pgn}=$self->{mw}->Toplevel();
		$self->{show_pgn}->geometry('+5+5');
		
		$self->{show_pgn_text}=$self->{show_pgn}->Scrolled('Text',%pgn_text_attr,-width=>110,-height=>30,-scrollbars=>'e',-insertontime=>0)->pack;
		
		$self->{show_pgn_text}->bind('<Button-1>',[\&show_pgn_clicked,$self]);
	}
	
	$self->{show_pgn}->configure(-title=>$pgn_dir);
	
	$self->{show_pgn_text}->delete('1.0','end');
	
	opendir(DIR,$pgn_dir);
	while($dir=readdir(DIR))
	{
		if( ($dir=~/\.pgn$/i) || opendir(DUMMY,"$pgn_dir/$dir") )
		{
			$self->{show_pgn_text}->insert('end',"$dir\n");
		}
	}
	
}

sub parse
{

	my $self=shift;
	my $dir=shift;
	
	my $pgn={dir=>$dir};
	open(PGN,"$self->{settings}->{pgn_dir}/$dir");
	while(my $line=<PGN>)
	{
		$pgn->{full_pgn}.=$line;
		
		$line=~s/[\n\r]//g;
		if($line=~/\[([^\]]+)\]/)
		{
			my $header=$1;
			my @header=split /\s+/,$header;
			my $key=$header[0];
			my $value=$header[1];$value=~s/\"//g;
			$pgn->{headers}->{$key}=$value;
		}
		else
		{
			$pgn->{body}.="$line ";
		}
	}
	close(PGN);
	
	# remove newlines, excess white spaces
	
	$pgn->{body}=~s/^\s+//g;
	$pgn->{body}=~s/\s+$//g;
	while($pgn->{body}=~s/  / /){};
	$pgn->{body}=~s/([0-9]+\.)\s+/$1/g;
	$pgn->{headers}->{game}=$pgn->{body};
	
	$pgn->{raw}=$pgn->{body};
	
	# establish reason

	my $reason;
	
	while($pgn->{raw}=~s/\{([^\}]*)\}//){$reason.=$1;}
	
	$reason=~tr/[A-Z]/[a-z]/;
	$reason=~s/^\s*//;
	$reason=$reason eq ''?' forced':" $reason";
	
	$pgn->{reason}=$reason;
	
	$pgn->{raw}=~s/[0-9]+\.//g;
	
	while($pgn->{raw}=~s/  / /){};
	
	#remove numerical result if any
	
	my @pgn_raw=split / /,$pgn->{raw};
	my $numresult=pop(@pgn_raw);
	
	if($numresult=~/[0-9\/\?]+\-[0-9\/\?]+$/)
	{
		$pgn->{numresult}=$numresult;
		$pgn->{raw}=join(' ',@pgn_raw);
	}
	
	return $pgn;
}

sub report
{

	my $self=shift;
	
	my $what=shift;

	$self->{status_line}->delete('1.0','end');
	$self->{status_line}->insert('end'," # $what");
	if($self->{settings}->{update_status_bar}){$self->{status_frame}->update;}
	
}

sub show_pgn_clicked
{

	shift;
	$self=shift;
	
	return if $self->{analysis_mode};
	
	my $coords=Utils::split_text_index($self->{show_pgn_text}->index('current'));
	
	my $line=$self->{show_pgn_text}->get("$coords->{y}.0","$coords->{y}.120");
	
	my $pgn_dir=$self->{settings}->{pgn_dir};
	if(!($pgn_dir=~/\/$/)){$pgn_dir.='/';}
	
	my $is_dir="$pgn_dir/$line";$is_dir=~s/\/\//\//g;
	
	if(opendir(NEWDIR,$is_dir))
	{
		if($line eq '.')
		{
		}
		elsif($line eq '..')
		{
			@pgn_dir=split /\//,$pgn_dir;
			if(@pgn_dir>1)
			{
				pop(@pgn_dir);
				$pgn_dir=join('/',@pgn_dir);
				if(!($pgn_dir=~/\/$/)){$pgn_dir.='/';}
				$self->{settings}->{pgn_dir}=$pgn_dir;
			}
		}
		else
		{
			$self->{settings}->{pgn_dir}=$is_dir;
		}
		$self->save_settings;
		$self->load_pgn(1);
		return;
	}
	
	$self->{show_pgn}->destroy;
	
	my $pgn=$self->parse($line);
	
	my @moves=split / /,$pgn->{raw};
	
	$self->reset_game;
	
	my $number_of_moves=@moves+0;
	my $cnt=1;
	my $success=0;
	
	my @failed;
	
	my $t0=[gettimeofday];
	
	my $show_legal=$self->{show_legal_moves};
	
	foreach(@moves)
	{
		my $pgn_move=$_;
		
		$pgn_move=~/^(.)/;
		
		my $piece=$1;
		
		my $is_black_turn=$self->{board}->{turn}==-1;
		
		if($piece=~/[NBRQK]/)
		{
			if($is_black_turn)
			{
				$piece=$Board::black_of_piece{$piece};
			}
		}
		elsif($piece eq 'O')
		{
			$piece=$is_black_turn?'k':'K';
		}
		else
		{
			$piece=$is_black_turn?'p':'P';
		}
	
		$self->{board}->{dont_check_pgn}=0;
		my $legal_algeb_moves=$self->{board}->legal_algeb_moves($piece);
		
		$self->{show_legal_moves}=0;
		
		my $found;
		
		foreach(keys(%{$legal_algeb_moves}))
		{
			my $full_algeb=$_;
			
			if($legal_algeb_moves->{$full_algeb}->{move}->{pgn_move} eq  $pgn_move)
			{
				$self->make_move($full_algeb);
				
				if(0)
				{
					$self->move_made;
					
					$self->{board_frame}->update;
				}
				
				$found=1;
				
			}
			
		}
		
		$success+=$found;
		
		if(!$found)
		{
			push(@failed,"$cnt $pgn_move");
		}
		
		my $failed;
		
		if(@failed>0)
		{
			$failed=" ( failed: ".join(' , ',@failed)." )";
		}
		
		my $elapsed=tv_interval($t0,[gettimeofday]);
		
		$self->report("processing move $cnt of $number_of_moves$failed ( elapsed $elapsed )");
		
		$cnt++;

	}
	
	$self->{show_legal_moves}=$show_legal;
	
	$self->{pgn_white}->delete('1.0','end');
	$self->{pgn_white}->insert('end',$pgn->{headers}->{White});
	$self->{pgn_white_elo}->delete('1.0','end');
	$self->{pgn_white_elo}->insert('end',$pgn->{headers}->{WhiteElo});
	$self->{pgn_black}->delete('1.0','end');
	$self->{pgn_black}->insert('end',$pgn->{headers}->{Black});
	$self->{pgn_black_elo}->delete('1.0','end');
	$self->{pgn_black_elo}->insert('end',$pgn->{headers}->{BlackElo});
	$self->{pgn_date}->delete('1.0','end');
	$self->{pgn_date}->insert('end',$pgn->{headers}->{Date});
	$self->{pgn_result}->delete('1.0','end');
	$self->{pgn_result}->insert('end',$pgn->{headers}->{Result});
	$self->{pgn_time_control}->delete('1.0','end');
	$self->{pgn_time_control}->insert('end',$pgn->{headers}->{TimeControl});
	$self->{pgn_ply_count}->delete('1.0','end');
	$self->{pgn_ply_count}->insert('end',$pgn->{headers}->{PlyCount});
	$self->{pgn_dir}->delete('1.0','end');
	$self->{pgn_dir}->insert('end',$line);
	
	$self->{mw}->configure(-title=>$line);
	
	$self->move_made;

	if($self->{settings}->{show_pgn})
	{
		$self->show_pgn($pgn->{full_pgn});
	}

}

my $depth_percent;

########################################################################

sub new

########################################################################

{

	shift;
	
	my $mw=shift;

	my $self={
	mw=>$mw,
	book=>{},
	after_move_book=>{}
	};
	
	bless $self;
	
	#########################################
	
	$self->load_settings;
	
	#########################################
	
	mkdir('Pgn');
	my @dir=split /\//,getcwd;
	if(!exists($self->{settings}->{pgn_dir})){$self->{settings}->{pgn_dir}=join('/',@dir).'/Pgn';}
	
		#########################################
		
		$self->{status_frame}=$mw->Frame(-padx=>5,-pady=>5,-borderwidth=>'4',-relief=>'raised')->grid(-row=>1,-column=>0,-columnspan=>4);
		
		$self->{status_line}=$self->{status_frame}->Text(-width=>80,-font=>[-size=>10],-height=>1,-insertontime=>0)->pack(-side=>'left');
		$self->{show_pgn_checkbox}=$self->{status_frame}->Checkbutton(-text => 'Show PGN', -onvalue => '1', -offvalue => '0',-command=>sub{$self->save_settings;},-variable => \$self->{settings}->{show_pgn})->pack(%button_pack_options,-side=>'left');
		$self->{update_status_bar_checkbox}=$self->{status_frame}->Checkbutton(-text => 'Update Status', -onvalue => '1', -offvalue => '0',-command=>sub{ $self->save_settings; },-variable => \$self->{settings}->{update_status_bar})->pack(%button_pack_options,-side=>'left');
		
		$self->{status_frame}->Label(-text=>'Dump depth')->pack(-side=>'left');
		
		my @dump_depth_options=map { $_*5+10 } 0..4;
		
		$self->{dump_depth_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@dump_depth_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{dump_depth}
		)->pack(%button_pack_options,-side=>'left');
		
		$self->{status_frame}->Label(-text=>'Depth%')->pack(-side=>'left');
		
		my @depth_percent_options=map { $_*10+10 } 0..9;
		
		$self->{depth_percent_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@depth_percent_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{depth_percent}
		   )->pack(%button_pack_options,-side=>'left');
		   
		$self->{status_frame}->Label(-text=>'Minimax after')->pack(-side=>'left');
		
		my @minimax_after_options=map { $_*5 } 1..10;
		
		$self->{minimax_after_optionmenu}=$self->{status_frame}->Optionmenu(
			-options => \@minimax_after_options,
			-command => sub { $self->save_settings; },
			-textvariable => \$self->{settings}->{minimax_after}
		)->pack(%button_pack_options,-side=>'left');
		
		$self->{board_frame}=$mw->Frame(-padx=>5,-pady=>5,-borderwidth=>'4',-relief=>'raised')->grid(-row=>0,-column=>0);
		
		$self->{engine_frame}=$self->{board_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{engine_line}=$self->{engine_frame}->Text(-width=>41,-font=>[-family=>'Courier',-size=>14,-weight=>'bold'],-height=>1,-insertontime=>0)->pack();
		$self->{engine_line}->tagConfigure('good',-foreground=>'#007f00');
		$self->{engine_line}->tagConfigure('neutral',-foreground=>'#0000ff');
		$self->{engine_line}->tagConfigure('bad',-foreground=>'#7f0000');
		
		$self->{engine_frame_inactive_background}=$self->{engine_frame}->cget(-background);
		
		$self->{engine_controls_frame}=$self->{engine_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{engine_analysis_button}=$self->{engine_controls_frame}->Button(-text=>'Start analysis',-command=>[\&change_analysis,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_minimax_button}=$self->{engine_controls_frame}->Button(-text=>'Minimax',-command=>[\&minimax_out,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{engine_on_checkbox}=$self->{engine_controls_frame}->Checkbutton(-text => 'Engine', -onvalue => '1', -offvalue => '0',-variable => \$self->{engine})->pack(%button_pack_options,-side=>'left');
		$self->{engine_go_button}=$self->{engine_controls_frame}->Button(-background=>'#afffaf',-activebackground=>'#afdfaf',-text=>'Go',-command=>[\&go,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_stop_button}=$self->{engine_controls_frame}->Button(-background=>'#ffafaf',-activebackground=>'#dfafaf',-text=>'Stop',-command=>[\&stop,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_make_button}=$self->{engine_controls_frame}->Button(-background=>'#afafff',-activebackground=>'#afafdf',-text=>'Make',-command=>[\&make_engine_move,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{engine_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{engine_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{canvas} = $self->{board_frame}->Canvas(
			-width  => $canvas_size,
			-height => $canvas_size,
			-borderwidth=>'4',
			-relief=>'raised'
		)->pack();
		
		$self->{canvas}->bind( 'draggable', '<1>'                   => [ \&drag_start , $self] );
		$self->{canvas}->bind( 'draggable', '<B1-Motion>'           => \&drag_during );
		$self->{canvas}->bind( 'draggable', '<Any-ButtonRelease-1>' => [ \&drag_end , $self ] );
		$self->{canvas}->bind( 'draggable', '<B1-Enter>' => undef );
		$self->{canvas}->bind( 'draggable', '<B1-Leave>' => undef );
		
		$self->{board_controls_frame}=$self->{board_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{board_controls_frame}->Button(-text=>'Flip',-command=>[\&flip_board,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<<',-command=>[\&del_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'>>',-command=>[\&undel_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Checkbutton(-text => 'Show legal', -onvalue => '1', -offvalue => '0',-variable => \$self->{show_legal_moves})->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'Copy PGN',-command=>[\&copy_pgn,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'Copy FEN',-command=>[\&copy_fen,$self])->pack(%button_pack_options,-side=>'left');
		$self->{board_controls_frame}->Button(-text=>'Reset',-command=>[\&reset_game,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{legal_moves_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3)->grid(-row=>0,-column=>1);
		
		$self->{legal_moves_text}=$self->{legal_moves_frame}->Scrolled('Text',-scrollbars=>'e',-width=>12,-height=>41,-insertontime=>0)->pack();
		
		$self->{save_load_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3)->grid(-row=>0,-column=>2);
		
		$self->{pgn_frame}=$self->{save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack;
		
		$self->{pgn_frame}->Button(-text=>'Load PGN',-command=>[\&load_pgn,$self])->pack(-pady=>1);
		
		$self->{pgn_frame}->Label(-text=>'White')->pack(-pady=>1);
		$self->{pgn_white}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'White Elo')->pack(-pady=>1);
		$self->{pgn_white_elo}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Black')->pack(-pady=>1);
		$self->{pgn_black}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Black Elo')->pack(-pady=>1);
		$self->{pgn_black_elo}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Date')->pack(-pady=>1);
		$self->{pgn_date}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Result')->pack(-pady=>1);
		$self->{pgn_result}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Time Control')->pack(-pady=>1);
		$self->{pgn_time_control}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'Ply Count')->pack(-pady=>1);
		$self->{pgn_ply_count}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		$self->{pgn_frame}->Label(-text=>'File')->pack(-pady=>1);
		$self->{pgn_dir}=$self->{pgn_frame}->Text(%pgn_header_text_attr)->pack();
		
		$self->{pgn_frame}->Button(-text=>"Save PGN",-command=>[\&copy_pgn,$self,1])->pack(-pady=>1);
		
		$self->{numbered_save_load_frame}=$self->{save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack;
		$self->{load_frame}=$self->{numbered_save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack(-side=>'left');
		$self->{save_frame}=$self->{numbered_save_load_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-padx=>3,-pady=>3)->pack(-side=>'left');
		
		for(my $i=1;$i<=5;$i++)
		{
			$self->{"save_".$i."_button"}=$self->{save_frame}->Button(-text=>"Save $i",-command=>[\&save_i,$self,$i])->pack;
			$self->{"load_".$i."_button"}=$self->{load_frame}->Button(-text=>"Load $i",-command=>[\&load_i,$self,$i])->pack;
		}
		
		$self->{book_frame}=$mw->Frame(-borderwidth=>'2',-relief=>'raised')->grid(-row=>0,-column=>3,-padx=>3);
		
		$self->{book_controls_frame}=$self->{book_frame}->Frame(-borderwidth=>'2',-relief=>'raised',-pady=>3)->pack();
		
		$self->{save_book_button}=$self->{book_controls_frame}->Button(-text=>'Save book',-command=>[\&save_book,$self])->pack(%button_pack_options,-side=>'left');
		$self->{save_book_button}=$self->{book_controls_frame}->Button(-text=>'Dump book',-command=>[\&dump_book,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'<<',-command=>[\&del_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'<',-command=>[\&del_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'>',-command=>[\&undel_move,$self])->pack(%button_pack_options,-side=>'left');
		$self->{book_controls_frame}->Button(-text=>'>>',-command=>[\&undel_all_moves,$self])->pack(%button_pack_options,-side=>'left');
		
		$self->{book_text}=$self->{book_frame}->Scrolled('Text',-font=>[-family=>'Courier',-size=>14],-scrollbars=>'e',-width=>50,-height=>18,-insertontime=>0)->pack(-pady=>5);
		$self->{book_text}->bind('<Button-1>',[\&book_clicked,$self]);
		
		$self->{game_frame}=$self->{book_frame}->Frame()->pack(-padx=>10,-pady=>5);
		
		$self->{game_text}=$self->{game_frame}->Scrolled('Text',-scrollbars=>'e',-padx=>5,-width=>78,-height=>10,-insertontime=>0)->pack();
		
		$self->{game_text}->tagConfigure('undel',-foreground=>'#ff0000');
		
		foreach(@comments)
		{
			$self->{book_text}->tagConfigure($comments->{$_}->{name},%{$comments->{$_}->{tags}},-font=>[-family=>'Courier',-size=>14,-weight=>'bold']);
		}
		
		$self->{book_text}->tagConfigure('back',-background=>'#cfcfcf');
		$self->{book_text}->tagConfigure('small',-font=>[-family=>'Courier',-size=>12]);
		$self->{book_text}->tagConfigure('margin',-lmargin1=>15);
		
		#########################################
	
	$self->reset;
	
	$self->load_book;
	
	$self->reset_game;
	
	$self->{mw}->after(500,[\&update_engine,$self]);
	
	return $self;
	
}

########################################################################

sub ij_to_screen_ij
{
	my $self=shift;
	
	my $ij=shift;
	
	if($self->{flip})
	{
		return({i=>7-$ij->{i},j=>7-$ij->{j}});
	}
	else
	{
		return $ij;
	}
}

sub screen_ij_to_ij
{
	my $self=shift;
	
	my $screen_ij=shift;
	
	if($self->{flip})
	{
		return({i=>7-$screen_ij->{i},j=>7-$screen_ij->{j}});
	}
	else
	{
		return $screen_ij;
	}
}

sub draw_board
{

	my $self=shift;
	
	if($self->{dont_draw}){return;}
	
	$self->{canvas}->delete('all');
	
	my @rep=split //,$self->{board}->board_rep;
	
	for(my $i=0;$i<8;$i++)
	{
		for(my $j=0;$j<8;$j++)
		{
			my $ij={i=>$i,j=>$j};
			
			my $screen_ij=$self->ij_to_screen_ij($ij);
			
			$self->{canvas}->createText(
			$screen_ij->{i}*$square_size+$margin+$square_size/2,$screen_ij->{j}*$square_size+$square_size/2+$margin,
			-text => $rep[$j*8+$i],
			-font => [ -family => 'Chess Merida', -size => $piece_size ],
			-tags       => ['draggable']
			);
		}
	}
	
	my $left=join(' ',map { sprintf "%-5s",$_->{full_algeb} } @{$self->{moves}});
	my $all=join(' ',(map { sprintf "%-5s",$_->{full_algeb} } @{$self->{moves}}),(reverse(map { sprintf "%-5s",,$_->{full_algeb} } @{$self->{unmoves}})));
	my $x1=length($left);
	$self->{game_text}->delete('1.0','end');
	$self->{game_text}->insert('end',$all);
	$self->{game_text}->tagAdd('undel',"1.$x1",'end');
	
}

###################################################################

package Main;

###################################################################

use Tk;

my $mw = new MainWindow( -title => "Atomic Chess" );

$mw->geometry("+0+40");

my $game=new Game($mw);

MainLoop;