<?php

namespace App\Http\Middleware;

use Closure;

class Statistics
{
    // function to verify that the current signed in user has the role of Restaurant (does not check approval)
    public function handle($request, Closure $next)
    {
        if (! ($request->user()) || ($request->user()->role != "Restaurant")) {
            return redirect("/restaurant");
        }
        return $next($request);
    }
}