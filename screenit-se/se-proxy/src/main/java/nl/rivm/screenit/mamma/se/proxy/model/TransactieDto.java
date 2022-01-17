package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.format.DateTimeFormatter;
import java.util.List;

public class TransactieDto
{
    private SETransactieType type;

    private Long clientId;

    private Long instellingGebruikerId;

    private String afspraakVanafDatum;

    private List<ActionDto> actions;

    public TransactieDto(SETransactieType type, Long clientId, Long instellingGebruikerId, List<ActionDto> actions, LocalDate afspraakVanafDatum)
    {
        this.type = type;
        this.clientId = clientId;
        this.instellingGebruikerId = instellingGebruikerId;
        this.actions = actions;
        this.afspraakVanafDatum = afspraakVanafDatum.format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    public SETransactieType getType()
    {
        return type;
    }

    public void setType(SETransactieType type)
    {
        this.type = type;
    }

    public Long getClientId()
    {
        return clientId;
    }

    public void setClientId(Long clientId)
    {
        this.clientId = clientId;
    }

    public Long getInstellingGebruikerId()
    {
        return instellingGebruikerId;
    }

    public void setInstellingGebruikerId(Long instellingGebruikerId)
    {
        this.instellingGebruikerId = instellingGebruikerId;
    }

    public List<ActionDto> getActions()
    {
        return actions;
    }

    public void setActions(List<ActionDto> actions)
    {
        this.actions = actions;
    }

    public String getAfspraakVanafDatum()
    {
        return afspraakVanafDatum;
    }

    public void setAfspraakVanafDatum(String afspraakVanafDatum)
    {
        this.afspraakVanafDatum = afspraakVanafDatum;
    }
}
