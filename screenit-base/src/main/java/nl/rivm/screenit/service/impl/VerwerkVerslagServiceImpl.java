
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.Set;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.cervix.CervixVerwerkVerslagService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.mamma.MammaVerwerkVerslagService;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VerwerkVerslagServiceImpl implements VerwerkVerslagService
{

	private static final Logger LOG = LoggerFactory.getLogger(VerwerkVerslagServiceImpl.class);

	@Autowired
	private String versionMapping;

	@Autowired(required = false)
	private ColonVerwerkVerslagService colonVerwerkVerslagService;

	@Autowired(required = false)
	private CervixVerwerkVerslagService cervixVerwerkVerslagService;

	@Autowired(required = false)
	private MammaVerwerkVerslagService mammaVerwerkVerslagService;

	@PostConstruct
	public void init()
	{
		if (StringUtils.isNotBlank(versionMapping))
		{
			String[] splittedVersionMapping = versionMapping.trim().split(";");
			for (String versionMap : splittedVersionMapping)
			{
				String[] splittedVersionMap = versionMap.split("\\|");

				VerslagGeneratie generatie = VerslagGeneratie.valueOf(splittedVersionMap[0]);
				String[] projectVersions = splittedVersionMap[1].split(",");
				if (projectVersions.length == 0)
				{
					VerslagProjectVersionMapping.get().addProjectVersion("", generatie);
				}
				else
				{
					for (String projectVersion : projectVersions)
					{
						VerslagProjectVersionMapping.get().addProjectVersion(projectVersion, generatie, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
					}
				}
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkInDossier(Verslag verslag)
	{
		switch (verslag.getType())
		{
		case MDL:
			colonVerwerkVerslagService.verwerkInDossier((MdlVerslag) verslag);
			break;
		case PA_LAB:
			break;
		case CERVIX_CYTOLOGIE:
			cervixVerwerkVerslagService.verwerkInDossier((CervixCytologieVerslag) verslag);
			break;
		case MAMMA_PA_FOLLOW_UP:
			mammaVerwerkVerslagService.verwerkVerslagInDossier((MammaFollowUpVerslag) verslag);
			break;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onAfterVerwerkVerslagContent(Verslag verslag)
	{
		switch (verslag.getType())
		{
		case MDL:
			colonVerwerkVerslagService.onAfterVerwerkVerslagContent((MdlVerslag) verslag);
			break;
		case PA_LAB:
			colonVerwerkVerslagService.onAfterVerwerkVerslagContent((PaVerslag) verslag);
			break;
		case CERVIX_CYTOLOGIE:
			cervixVerwerkVerslagService.onAfterVerwerkVerslagContent((CervixCytologieVerslag) verslag);
			break;
		case MAMMA_PA_FOLLOW_UP:
			mammaVerwerkVerslagService.onAfterVerwerkVerslagContent((MammaFollowUpVerslag) verslag);
			break;
		}
	}

	@Override
	public ScreeningRonde getValideScreeningsRonde(VerslagType type, Client client, Verslag olderVerslag, Date onderzoeksdatum)
	{
		switch (type)
		{
		case MDL:
		case PA_LAB:
			return colonVerwerkVerslagService.getValideScreeningsRonde(client, olderVerslag, onderzoeksdatum);
		case CERVIX_CYTOLOGIE:
			break;
		case MAMMA_PA_FOLLOW_UP:
			return mammaVerwerkVerslagService.getValideScreeningsRonde(client, onderzoeksdatum);
		}
		return null;
	}

	@Override
	public void valideerVerslagVoorAfronden(Verslag verslag, Set<Antwoord<?>> antwoorden, InstellingGebruiker instellingGebruiker)
	{
		switch (verslag.getType())
		{
		case MDL:
			colonVerwerkVerslagService.valideerVerslagVoorAfronden((MdlVerslag) verslag, antwoorden, instellingGebruiker);
			break;
		case PA_LAB:
			colonVerwerkVerslagService.valideerVerslagVoorAfronden((PaVerslag) verslag, instellingGebruiker);
			break;
		case CERVIX_CYTOLOGIE:
			break;
		case MAMMA_PA_FOLLOW_UP:
			mammaVerwerkVerslagService.valideerVerslagVoorAfronden((MammaFollowUpVerslag) verslag, instellingGebruiker);
			break;
		}
	}

	@Override
	public void ontkoppelOfVerwijderComplicaties(MdlVerslag mdlVerslag)
	{
		colonVerwerkVerslagService.ontkoppelOfVerwijderComplicaties(mdlVerslag);
	}

}
