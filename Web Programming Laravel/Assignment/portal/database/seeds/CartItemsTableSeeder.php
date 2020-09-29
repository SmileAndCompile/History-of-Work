<?php

use Illuminate\Database\Seeder;

class CartItemsTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */

    // function to seed Cart_Items into the Cart_Items table of the database
    public function run()
    {
        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '13',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '10',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '6',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '2',
            'dish_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('cart_items')->insert([
            'user_id' => '1',
            'dish_id' => '7',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 
    }
}
