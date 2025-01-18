public class Hero extends Character {
    private int exp, level;

    public Hero(String name, int hp, int damage) {
        super(name, hp, damage);
        this.exp = 0;
        this.level = 0;
    }

    public int getExp() {
        return exp;
    }

    public int getLevel() {
        return level;
    }
   
    public void levelUp(int x) {
        this.level += x;
        this.incDamage(x * 5);
        this.incHP(x * 10);
        System.out.println(this.getName() + 
            " has leveled up to level " + this.getLevel() + "!");
    }

    public void incExp(int x) {
        this.exp += x;
        System.out.println("Current exp: " + this.exp);
        this.levelUp((this.getExp() - this.getLevel()*10) / 10);
    }
}

