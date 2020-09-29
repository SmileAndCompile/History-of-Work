<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class CartItem extends Model
{
    protected $fillable = [
        'user_id', 'dish_id'
    ];

    function users(){
        return $this->hasMany('App\User', 'user_id');
    }

    function dishes(){
        return $this->hasMany('App\Dish', 'dish_id');
    }
}
