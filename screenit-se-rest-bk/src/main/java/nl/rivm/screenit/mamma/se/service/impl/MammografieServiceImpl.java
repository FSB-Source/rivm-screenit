package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.se.dto.actions.DensiteitMetingActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.MammografieOpslaanDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammografieService;
import nl.rivm.screenit.mamma.se.service.dtomapper.AfbeeldingDtoMapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.repository.mamma.MammaScreeningRondeRepository;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.mamma.MammaBaseAnnotatieAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MammografieServiceImpl implements MammografieService
{
	private final HibernateService hibernateService;

	private final MammaBaseAnnotatieAfbeeldingService baseAnnotatieAfbeeldingService;

	private final MammaBaseFactory baseFactory;

	private final MammaAfspraakService afspraakService;

	private final MammaScreeningRondeRepository screeningRondeRepository;

	private final OrganisatieParameterService organisatieParameterService;

	private final MammaBaseDense2Service dense2Service;

	@Override
	@Transactional
	public void opslaanEnStatusovergang(MammografieOpslaanDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		var onderzoek = getOnderzoek(action, afspraak);
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var mammografie = getOfMaakMammografie(onderzoek);

		verwerkVisueleInspectieAfbeelding(onderzoek, action);
		mammografieVerwerken(mammografie, instellingGebruiker, transactieDatumTijd);

		hibernateService.saveOrUpdateAll(dossier, onderzoek, mammografie);
	}

	@Override
	@Transactional
	public void opslaan(MammografieOpslaanDto action, InstellingGebruiker instellingGebruiker)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		var onderzoek = getOnderzoek(action, afspraak);
		var mammografie = getOfMaakMammografie(onderzoek);

		verwerkVisueleInspectieAfbeelding(onderzoek, action);
		hibernateService.saveOrUpdateAll(onderzoek, mammografie);
	}

	private MammaMammografie getOfMaakMammografie(MammaOnderzoek onderzoek)
	{
		var mammografie = onderzoek.getMammografie();
		if (mammografie == null)
		{
			mammografie = baseFactory.maakMammografie(onderzoek, null, null);

			onderzoek.setMammografie(mammografie);
		}
		return mammografie;
	}

	private void mammografieVerwerken(MammaMammografie mammografie, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		mammografie.setAfgerondDoor(instellingGebruiker);
		mammografie.setAfgerondOp(DateUtil.toUtilDate(transactieDatumTijd));
	}

	private void verwerkVisueleInspectieAfbeelding(MammaOnderzoek onderzoek, MammografieOpslaanDto action)
	{
		var afbeeldingDtoMapper = new AfbeeldingDtoMapper();
		var mammografie = onderzoek.getMammografie();
		var visueleInspectieAfbeelding = mammografie.getVisueleInspectieAfbeelding();
		if (action.getMammografie().getVisueleInspectieAfbeelding() != null
			&& action.getMammografie().getVisueleInspectieAfbeelding().getIconen() != null && !action.getMammografie().getVisueleInspectieAfbeelding().getIconen().isEmpty())
		{
			if (visueleInspectieAfbeelding == null)
			{
				visueleInspectieAfbeelding = new MammaAnnotatieAfbeelding();
				mammografie.setVisueleInspectieAfbeelding(visueleInspectieAfbeelding);
				hibernateService.saveOrUpdate(visueleInspectieAfbeelding);
			}

			var afbeeldingDto = action.getMammografie().getVisueleInspectieAfbeelding();
			var iconen = afbeeldingDto.getIconen().stream().map(afbeeldingDtoMapper::icoonDtoToAnnotatieIcoon).collect(Collectors.toList());
			baseAnnotatieAfbeeldingService.updateIconenInAfbeelding(iconen, visueleInspectieAfbeelding);
		}
		else
		{
			if (visueleInspectieAfbeelding != null)
			{
				hibernateService.delete(visueleInspectieAfbeelding);
				mammografie.setVisueleInspectieAfbeelding(null);
			}
		}
		hibernateService.saveOrUpdate(mammografie);
	}

	private MammaOnderzoek getOnderzoek(MammografieOpslaanDto action, MammaAfspraak afspraak)
	{
		if (afspraak == null)
		{
			throw new IllegalStateException("Afspraak id " + action.getAfspraakId() + " bestaat niet");
		}

		var onderzoek = afspraak.getOnderzoek();

		if (onderzoek == null || onderzoek.isDoorgevoerd())
		{
			throw new IllegalStateException("Opslaan visuele inspectie alleen mogelijk bij niet doorgevoerd onderzoek of onderzoek is null.");
		}
		return onderzoek;
	}

	@Override
	@Transactional
	public void densiteitMetingOpslaan(DensiteitMetingActionDto action)
	{
		var screeningRonde = screeningRondeRepository.findByUitnodigingsNr(action.getAccessionNumber())
			.orElseThrow(() -> new IllegalStateException("Screeningronde voor densemeting niet gevonden voor accessionnumber: " + action.getAccessionNumber()));

		var densiteit = action.getDensiteit();
		var client = screeningRonde.getDossier().getClient();
		if (magDensiteitOpslaan(densiteit, client))
		{
			getOfMaakMammografie(screeningRonde.getLaatsteOnderzoek()).setDensiteit(densiteit);
		}
	}

	private boolean magDensiteitOpslaan(MammaDenseWaarde densiteit, Client client)
	{
		var initieleMetingOpslaan = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_DENSE_2_INITIELE_METING_OPSLAAN, false);
		var clientInDense2Project = dense2Service.clientZitInDense2Project(client, dense2Service.getConfiguratie());
		return clientInDense2Project || (Boolean.TRUE.equals(initieleMetingOpslaan) && densiteit == MammaDenseWaarde.D);
	}
}
