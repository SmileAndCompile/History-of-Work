<?php

use Illuminate\Database\Seeder;

use Carbon\Carbon;

class OrdersTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */

    // function to seed Orders into the Orders table of the database
    public function run()
    {
        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Chicken Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(26),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '2',
            'dish' => 'Chicken Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(16),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Vegan Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(16),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Vegan Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(6),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Vegan Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(13),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Beef Burger',
            'price' => '9.50',
            'orderDate' => Carbon::now()->subDays(30),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Chicken Burger',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(30),
            'rest_id'=> '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '2',
            'dish' => 'Cheeseburger',
            'price' => '3.50',
            'orderDate' => Carbon::now()->subDays(6),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Happy Meal',
            'price' => '5.0',
            'orderDate' => Carbon::now()->subDays(13),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Big Mac',
            'price' => '10.50',
            'orderDate' => Carbon::now()->subDays(20),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '2',
            'dish' => 'Big Mac',
            'price' => '5.50',
            'orderDate' => Carbon::now()->subDays(27),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'McChicken',
            'price' => '5.0',
            'orderDate' => Carbon::now()->subDays(34),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '1',
            'dish' => 'Big Mac',
            'price' => '5.50',
            'orderDate' => Carbon::now()->subDays(41),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '2',
            'dish' => 'Big Mac',
            'price' => '5.50',
            'orderDate' => Carbon::now()->subDays(48),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('orders')->insert([
            'user_id' => '2',
            'dish' => 'Hamburger',
            'price' => '3.00',
            'orderDate' => Carbon::now(),
            'rest_id'=> '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 
    }
}
