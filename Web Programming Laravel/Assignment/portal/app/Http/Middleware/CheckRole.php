<?php

namespace App\Http\Middleware;

use Closure;

class CheckRole
{
    // function to verify a users role (Administrator, Customer or Restaurant). If the User is a Restaurant - we also check they are approved.
    public function handle($request, Closure $next, $role)
    {
        if (! ($request->user()) || ($request->user()->role != $role)) {
            return redirect("/restaurant");
        } else {
            if ($role == "Restaurant" && ($request->user()->approval != TRUE)){
                return redirect("/restaurant");
            }
        }
        return $next($request);
    }
}