<?php

namespace App\Http\Middleware;

use Closure;

use App\Dish;

class Dishes
{
    // function to verify that the current user signed in is a restaurant and the valid owner of a dish (i.e for editing / deleting a dish)
    public function handle($request, Closure $next)
    {
        $id = $request->route('dish');
        if (! $id || ! $request->user()){
            return redirect("/restaurant");
        } else {
            $check = Dish::whereRaw("id == $id")->select('user_id')->get();
        }
        if (count($check) < 1 || $check[0]->user_id != $request->user()->id || $request->user()->role != 'Restaurant' || $request->user()->approval != TRUE) {
            return redirect("/restaurant");
        }

        return $next($request);
    }
}