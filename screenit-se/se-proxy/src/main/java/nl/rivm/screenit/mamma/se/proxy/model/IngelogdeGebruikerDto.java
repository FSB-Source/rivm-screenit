package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

public class IngelogdeGebruikerDto
{
    private String gebruikersnaam;

    private String wachtwoord;

    private LocalDate laatsteInlog;

    private String yubikeyIdentificatie;

    private String loginResponse;

    private Long accountId;

    public IngelogdeGebruikerDto(String gebruikersnaam, String wachtwoord, LocalDate laatsteInlog, String yubikeyIdentificatie, String loginResponse, Long accountId)
    {
        this.gebruikersnaam = gebruikersnaam;
        this.wachtwoord = wachtwoord;
        this.laatsteInlog = laatsteInlog;
        this.yubikeyIdentificatie = yubikeyIdentificatie;
        this.loginResponse = loginResponse;
        this.accountId = accountId;
    }

    public String getGebruikersnaam()
    {
        return gebruikersnaam;
    }

    public void setGebruikersnaam(String gebruikersnaam)
    {
        this.gebruikersnaam = gebruikersnaam;
    }

    public String getWachtwoord()
    {
        return wachtwoord;
    }

    public void setWachtwoord(String wachtwoord)
    {
        this.wachtwoord = wachtwoord;
    }

    public LocalDate getLaatsteInlog()
    {
        return laatsteInlog;
    }

    public void setLaatsteInlog(LocalDate laatsteInlog)
    {
        this.laatsteInlog = laatsteInlog;
    }

    public String getYubikeyIdentificatie()
    {
        return yubikeyIdentificatie;
    }

    public void setYubikeyIdentificatie(String yubikeyIdentificatie)
    {
        this.yubikeyIdentificatie = yubikeyIdentificatie;
    }

    public String getLoginResponse()
    {
        return loginResponse;
    }

    public void setLoginResponse(String loginResponse)
    {
        this.loginResponse = loginResponse;
    }

    public Long getAccountId()
    {
        return accountId;
    }

    public void setAccountId(Long accountId)
    {
        this.accountId = accountId;
    }
}
