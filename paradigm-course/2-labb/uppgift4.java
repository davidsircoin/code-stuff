public class uppgift4 {
    public static void main(String[] args) {
        Hero aragorn      = new Hero("Aragorn",100,30);
        Monster smallorc  = new Monster("Small Orc",50,100,15);
        Monster bigorc    = new Monster("Big Orc",70,15,25);
        Monster witchking = new Monster("Witch King",300,50,100);

        Game game = new Game();

        game.fight(aragorn,smallorc);
        game.fight(aragorn,bigorc);
        game.fight(aragorn,witchking);
    }
}

