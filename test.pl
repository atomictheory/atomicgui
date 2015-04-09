use Tk;

my $mw=new MainWindow;

my $toplevel;

my $toplevel_open=0;

my $status=$mw->Label(-text=>'Toplevel Closed :(')->pack;

$mw->repeat(1000,sub
{
	$status->configure(-text=>Exists($toplevel)?
		'Toplevel Open :)':'Toplevel Closed :(');
}
);

sub close_toplevel
{
	return if !Exists($toplevel);
	$toplevel->destroy;
}

$mw->Button(-text=>'Open Toplevel',-command=>
	sub {
		$toplevel=$mw->Toplevel();
	}
)->pack;

$mw->Button(-text=>'Close Toplevel',-command=>\&close_toplevel)->pack;

MainLoop;