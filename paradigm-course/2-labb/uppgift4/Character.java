public class Character {
    private String name;
    private int hp, damage;

    public Character(String name, int hp, int damage) {
        this.name = name;
        this.hp = hp;
        this.damage = damage;
        }

    public String getName() {
        return name;
    }
    
    public int getDamage() {
       return damage;
    }
    
    public int getHP() {
       return hp;
    }
    
    public boolean isDead() {
       if (hp <= 0) {
          return true;
       }
       return false;
    }
    
    public void incDamage(int x) {
       this.damage += x;
    }
    
    public void incHP(int x) {
       this.hp += x;
    }
    
    public void decHP(int x) {
       hp -= x;

    }
    

}



