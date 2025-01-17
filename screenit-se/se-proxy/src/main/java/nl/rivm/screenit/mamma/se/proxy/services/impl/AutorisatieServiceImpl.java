package nl.rivm.screenit.mamma.se.proxy.services.impl;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import nl.rivm.screenit.mamma.se.proxy.model.AutorisatieDto;
import nl.rivm.screenit.mamma.se.proxy.model.IngelogdeGebruikerDto;
import nl.rivm.screenit.mamma.se.proxy.model.SERechtDto;
import nl.rivm.screenit.mamma.se.proxy.services.AutorisatieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.time.LocalDate;

@Service
public class AutorisatieServiceImpl implements AutorisatieService
{
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public boolean isGeautoriseerdVoorInloggen(IngelogdeGebruikerDto ingelogdeGebruikerDto) throws IOException
    {
        AutorisatieDto responseObject = objectMapper.readValue(ingelogdeGebruikerDto.getLoginResponse(), AutorisatieDto.class);
        SERechtDto inschrijven = responseObject.getInschrijvenRecht();
        SERechtDto connectieStatus = responseObject.getConnectiestatusRecht();
        return isGeautoriseerd(inschrijven) || isGeautoriseerd(connectieStatus);
    }

    @Override
    public boolean isGeautoriseerd(SERechtDto seRechtDto)
    {
        return seRechtDto.isAuthorized() && (seRechtDto.getEindDatum() == null || DateUtil.getCurrentDateTime().toLocalDate().compareTo(LocalDate.parse(seRechtDto.getEindDatum())) <= 0);
    }

}
