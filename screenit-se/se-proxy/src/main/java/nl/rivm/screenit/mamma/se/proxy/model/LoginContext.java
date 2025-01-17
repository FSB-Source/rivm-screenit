package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

public class LoginContext 
{
    private String gebruikersnaam;

    private String plainWachtwoord;

    private String encryptedWachtwoord;

    private Long accountId;

    private String yubikeyIdentificatie;

    public String getGebruikersnaam() 
    {
        return gebruikersnaam;
    }

    public void setGebruikersnaam(String gebruikersnaam)
    {
        this.gebruikersnaam = gebruikersnaam;
    }

    public String getPlainWachtwoord() 
    {
        return plainWachtwoord;
    }

    public void setPlainWachtwoord(String plainWachtwoord) 
    {
        this.plainWachtwoord = plainWachtwoord;
    }

    public String getEncryptedWachtwoord() 
    {
        return encryptedWachtwoord;
    }

    public void setEncryptedWachtwoord(String encryptedWachtwoord) 
    {
        this.encryptedWachtwoord = encryptedWachtwoord;
    }

    public Long getAccountId()
    {
        return accountId;
    }

    public void setAccountId(Long accountId)
    {
        this.accountId = accountId;
    }

    public String getYubikeyIdentificatie()
    {
        return yubikeyIdentificatie;
    }

    public void setYubikeyIdentificatie(String yubikeyIdentificatie)
    {
        this.yubikeyIdentificatie = yubikeyIdentificatie;
    }
}
