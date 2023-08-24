package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.MammaHuisartsDto;
import nl.rivm.screenit.mamma.se.dto.actions.InschrijvenDto;
import nl.rivm.screenit.mamma.se.dto.actions.SetEmailAdresDto;
import nl.rivm.screenit.mamma.se.service.InschrijvenService;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.dtomapper.HuisartsDtoMapper;
import nl.rivm.screenit.mamma.se.service.dtomapper.TijdelijkAdresDtoMapper;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EmailUtil;
import nl.rivm.screenit.util.TelefoonnummerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class InschrijvenServiceImpl implements InschrijvenService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BezwaarService bezwaarService;

	@Autowired
	private BriefHerdrukkenService briefHerdrukkenService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private ClientService clientService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void inschrijven(InschrijvenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd, MammaScreeningsEenheid screeningsEenheid)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		baseAfmeldService.heraanmeldenAlsClientAfgemeldIs(dossier);
		afspraakWijzigen(action, afspraak, instellingGebruiker);
		afspraakInschrijven(afspraak, instellingGebruiker, transactieDatumTijd);
		opslaanClientgegevens(action, dossier.getClient(), instellingGebruiker, transactieDatumTijd, screeningsEenheid);
		hibernateService.saveOrUpdate(afspraak);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void inschrijvingWijzigen(InschrijvenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd, MammaScreeningsEenheid screeningsEenheid)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		baseAfmeldService.heraanmeldenAlsClientAfgemeldIs(dossier);
		afspraakWijzigen(action, afspraak, instellingGebruiker);
		opslaanClientgegevens(action, dossier.getClient(), instellingGebruiker, transactieDatumTijd, screeningsEenheid);
		hibernateService.saveOrUpdate(afspraak);
	}

	@Override
	public List<MammaHuisartsDto> getAllHuisartsen()
	{
		Map<String, Boolean> parameters = new HashMap<>();
		parameters.put("verwijderd", Boolean.FALSE);
		List<EnovationHuisarts> enovationHuisartsen = hibernateService.getByParameters(EnovationHuisarts.class, parameters);
		return enovationHuisartsen.stream().map(HuisartsDtoMapper::createMammaHuisarsDto).collect(Collectors.toList());
	}

	@Override
	public void setEmailAdres(SetEmailAdresDto setEmailAdresDto, InstellingGebruiker ingelogdeGebruiker)
	{

	}

	private void huisartsSelecteren(Long enovationHuisartsId, MammaGeenHuisartsOption geenHuisartsOption, MammaScreeningRonde screeningsRonde)
	{
		if (enovationHuisartsId == null && geenHuisartsOption == null)
		{
			throw new IllegalStateException("Huisarts is verplicht bij inschrijven.");
		}
		if (enovationHuisartsId != null)
		{
			screeningsRonde.setGeenHuisartsOptie(null);
			screeningsRonde.setHuisarts(hibernateService.load(EnovationHuisarts.class, enovationHuisartsId));
		}
		else
		{
			screeningsRonde.setHuisarts(null);
			screeningsRonde.setGeenHuisartsOptie(geenHuisartsOption);
		}
		screeningsRonde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
	}

	private void vraagBezwaarAan(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker)
	{
		Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();

		if (Boolean.FALSE.equals(afspraak.getBezwaarAangevraagd()))
		{
			BezwaarBrief bezwaarBrief = bezwaarService.getNogNietVerwerkteBezwaarBrief(client.getBezwaarMomenten());
			if (bezwaarBrief == null)
			{
				BezwaarMoment nieuwBezwaarMoment = new BezwaarMoment();
				nieuwBezwaarMoment.setClient(client);
				nieuwBezwaarMoment.setManier(ClientContactManier.AANVRAAG_FORMULIER);
				bezwaarService.bezwaarAanvragen(client, nieuwBezwaarMoment);
			}
			else
			{
				briefHerdrukkenService.opnieuwAanmaken(bezwaarBrief, instellingGebruiker);
			}
			afspraak.setBezwaarAangevraagd(true);
		}
	}

	private void afspraakWijzigen(InschrijvenDto action, MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker)
	{
		afspraak.setIdentificatiesoort(action.getIdentificatiesoort());
		afspraak.setIdentificatienummer(action.getIdentificatienummer());
		if (action.getBezwaarAangevraagd())
		{
			vraagBezwaarAan(afspraak, instellingGebruiker);
		}
		huisartsSelecteren(action.getHuisartsId(), action.getGeenHuisartsOptie(), afspraak.getUitnodiging().getScreeningRonde());
	}

	private void afspraakInschrijven(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		afspraak.setStatus(MammaAfspraakStatus.INGESCHREVEN);
		afspraak.setIngeschrevenOp(DateUtil.toUtilDate(transactieDatumTijd));
		afspraak.setIngeschrevenDoor(instellingGebruiker);
	}

	private void opslaanClientgegevens(InschrijvenDto inschrijvenDto, Client client, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd,
		MammaScreeningsEenheid screeningsEenheid)
	{
		GbaPersoon persoon = client.getPersoon();
		if (inschrijvenDto.getTijdelijkAdres() != null)
		{
			opslaanTijdelijkAdres(inschrijvenDto, persoon);
		}

		valideerEnZetEmailadres(inschrijvenDto, persoon);
		valideerEnZetMobielnummer(inschrijvenDto, persoon);
		valideerEnZetExtraTelefoonnummer(inschrijvenDto, persoon);
		clientService.saveContactGegevens(client, instellingGebruiker, screeningsEenheid, transactieDatumTijd);
	}

	private void valideerEnZetEmailadres(InschrijvenDto inschrijvenDto, GbaPersoon persoon)
	{
		var emailadres = StringUtils.trimToNull(inschrijvenDto.getEmailadres());
		if (emailadres == null || EmailUtil.isCorrectEmailadres(emailadres))
		{
			persoon.setEmailadres(emailadres);
		}
		else
		{
			throw new IllegalStateException("E-mailadres is niet correct");
		}
	}

	private void valideerEnZetMobielnummer(InschrijvenDto inschrijvenDto, GbaPersoon persoon)
	{
		var mobielnummer = StringUtils.trimToNull(inschrijvenDto.getTelefoonnummer1());
		if (mobielnummer == null || TelefoonnummerUtil.isCorrectNederlandsMobielNummer(mobielnummer))
		{
			persoon.setTelefoonnummer1(mobielnummer);
		}
		else
		{
			throw new IllegalStateException("Mobiel nummer is niet correct");
		}
	}

	private void valideerEnZetExtraTelefoonnummer(InschrijvenDto inschrijvenDto, GbaPersoon persoon)
	{
		var extraTelefoonnummer = StringUtils.trimToNull(inschrijvenDto.getTelefoonnummer2());
		if (extraTelefoonnummer == null || TelefoonnummerUtil.isCorrectTelefoonnummer(extraTelefoonnummer))
		{
			persoon.setTelefoonnummer2(extraTelefoonnummer);
		}
		else
		{
			throw new IllegalStateException("Extra nummer is niet correct");
		}
	}

	private void opslaanTijdelijkAdres(InschrijvenDto inschrijvenDto, GbaPersoon persoon)
	{
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			tijdelijkAdres = new TijdelijkAdres();
			persoon.setTijdelijkAdres(tijdelijkAdres);
		}
		new TijdelijkAdresDtoMapper().updateTijdelijkAdres(tijdelijkAdres, inschrijvenDto.getTijdelijkAdres());
		hibernateService.saveOrUpdate(tijdelijkAdres);
	}
}
