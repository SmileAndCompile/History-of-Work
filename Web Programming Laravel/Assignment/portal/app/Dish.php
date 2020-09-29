<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Dish extends Model
{
    /**
     * The attributes that are mass assignable.
     *
     * @var array
     */
    protected $fillable = [
        'name', 'price', 'image', 'user_id', 'created_at', 'updated_at'
    ];

    function user(){
        return $this->belongsTo('App\User', 'user_id');
    }

    function cart_item(){
        return $this->belongsTo('App\CartItem');
    }
}
