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

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.mamma.se.dao.ClientIdentificatie;
import nl.rivm.screenit.mamma.se.dao.MammaAfsprakenDao;
import nl.rivm.screenit.mamma.se.dto.AfspraakSeDto;
import nl.rivm.screenit.mamma.se.dto.ClientSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.VorigOnderzoekDto;
import nl.rivm.screenit.mamma.se.service.DaglijstService;
import nl.rivm.screenit.mamma.se.service.dtomapper.AfspraakDtoMapper;
import nl.rivm.screenit.mamma.se.service.dtomapper.VorigOnderzoekDtoMapper;
import nl.rivm.screenit.mamma.se.websocket.socket.SeProxyWebsocket;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class DaglijstServiceImpl implements DaglijstService
{

	@Autowired
	private MammaAfsprakenDao afsprakenDao;

	@Autowired
	private MammaBaseAfspraakDao baseAfspraakDao;

	@Autowired
	private MammaBaseDossierService baseDossierService;

	@Autowired
	private MammaBaseBeoordelingService beoordelingService;

	@Autowired
	private MammaBaseOnderzoekService mammaBaseOnderzoekService;

	@Autowired
	private MammaBaseBeoordelingService mammaBaseBeoordelingService;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private String applicationEnvironment;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SeProxyWebsocket seProxyWebsocket;

	@Autowired
	private MammaBaseScreeningrondeService baseScreeningrondeService;

	private final AfspraakDtoMapper afspraakDtoMapper = new AfspraakDtoMapper();

	private final VorigOnderzoekDtoMapper vorigOnderzoekDtoMapper = new VorigOnderzoekDtoMapper();

	@Override
	public List<AfspraakSeDto> readDaglijst(LocalDate datum, String seCode)
	{
		var afspraakDtos = baseAfspraakDao
			.getAfspraken(seCode, datum, datum, MammaAfspraakStatus.NIET_GEANNULEERD.toArray(new MammaAfspraakStatus[] {}))
			.stream()
			.filter(afspraak -> afspraak.getId().equals(afspraak.getUitnodiging().getLaatsteAfspraak().getId())
				&& afspraak.getUitnodiging().getScreeningRonde().getStatus() == ScreeningRondeStatus.LOPEND)
			.map(this::createAfspraakDto)
			.collect(Collectors.toList());

		if (afsprakenKunnenGeopendWordenOpSe(datum))
		{
			setDefaultIdentificaties(afspraakDtos);
		}

		return afspraakDtos;
	}

	@Override
	public void verstuurUpdate(String seCodeEnDatum)
	{
		LOG.info("SE met code en datum " + seCodeEnDatum + " moet de daglijst opnieuw binnenhalen.");
		seProxyWebsocket.sendDaglijstUpdate(seCodeEnDatum);
	}

	private AfspraakSeDto createAfspraakDto(MammaAfspraak afspraak)
	{
		var afspraakSeDto = afspraakDtoMapper.createAfspraakSeDto(afspraak);
		updateAfspraakDtoBijzonderhedenZelfdeRonde(afspraak, afspraakSeDto);
		updateAfspraakDtoMetOpkomstTellers(afspraak, afspraakSeDto);
		updateClientDtoMetVorigeOnderzoeken(afspraak, afspraakSeDto.getClient());
		return afspraakSeDto;
	}

	private void updateClientDtoMetVorigeOnderzoeken(MammaAfspraak afspraak, ClientSeDto clientSeDto)
	{
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		var daglijstSe = afspraak.getStandplaatsPeriode().getScreeningsEenheid();

		clientSeDto.setJaarLaatsteVerwijzing(baseScreeningrondeService.getJaarLaatsteVerwijzing(dossier.getClient()));
		clientSeDto.setVorigeOnderzoeken(
			baseDossierService.laatste3AfgerondeRondesMetOnderzoek(dossier)
				.map(ronde -> createVorigOnderzoekDto(ronde, daglijstSe))
				.filter(Objects::nonNull).collect(Collectors.toList()));
	}

	private VorigOnderzoekDto createVorigOnderzoekDto(MammaScreeningRonde ronde, MammaScreeningsEenheid daglijstSe)
	{
		try
		{
			return vorigOnderzoekDtoMapper.createVorigOnderzoekDto(ronde, beoordelingService, mammaBaseOnderzoekService);
		}
		catch (Exception exception)
		{
			LOG.error(exception.getMessage(), exception);
			foutTijdensVorigOnderzoekMappen(ronde, daglijstSe);
			return null;
		}
	}

	private void foutTijdensVorigOnderzoekMappen(MammaScreeningRonde ronde, MammaScreeningsEenheid daglijstSe)
	{
		Client client = null;
		try
		{
			client = ronde.getDossier().getClient();
		}
		catch (Exception exception)
		{
			LOG.error(String.format("Exception bij opbouwen logmelding tijdens het mappen van een vorig onderzoek, ronde id: %s", ronde.getId()));
			LOG.error(exception.getMessage(), exception);
		}
		var clientId = client != null ? client.getId().toString() : "onbekende client";
		var seCode = daglijstSe != null ? daglijstSe.getCode() : "onbekende SE";
		LOG.error(String.format("Fout tijdens opbouwen vorig onderzoek (ronde %s) van client: %s in %s", ronde.getId(), clientId, seCode));
		sendErrorEmail(seCode);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_SE_DAGLIJST_OPBOUWEN_ERROR, daglijstSe, null, client, "Neem contact op met Topicus", currentDateSupplier.getLocalDateTime(),
			Bevolkingsonderzoek.MAMMA);
	}

	private void sendErrorEmail(String se)
	{
		var emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
		if (emailadressen != null)
		{
			var subject = String.format("ScreenIT foutieve historische data op %s", applicationEnvironment);
			var content = String.format("Bij het maken van een daglijst voor %s is een fout opgetreden. <br />" + 
				"Dit heeft tot gevolg dat de historische rondes bij een onderzoek niet getoond konden worden. <br />" + 
				"Controleer in de applicatielogging van ScreenIT welke client het betreft en neem contact op met Topicus. <br />" +
				"Zoek op 'Fout tijdens vorig onderzoek opbouwen' als gebeurtenis in algemeen>logging inzien", se);
			mailService.queueMailAanProfessional(emailadressen, subject, content, MailPriority.HIGH);
		}
	}

	private boolean afsprakenKunnenGeopendWordenOpSe(LocalDate afspraakDatum)
	{
		var vandaag = currentDateSupplier.getLocalDate();
		return !afspraakDatum.isBefore(vandaag);
	}

	private void updateAfspraakDtoBijzonderhedenZelfdeRonde(MammaAfspraak afspraak, AfspraakSeDto afspraakSeDto)
	{

		var eerderOnderbrokenInZelfdeRonde = afspraak.getUitnodiging().getScreeningRonde().getUitnodigingen().stream()
			.flatMap(uitnodiging -> uitnodiging.getAfspraken().stream())
			.filter(rondeAfspraak -> rondeAfspraak.getOnderzoek() != null && !rondeAfspraak.getId().equals(afspraak.getId()))
			.anyMatch(rondeAfspraak -> rondeAfspraak.getOnderzoek().getStatus() == MammaOnderzoekStatus.ONDERBROKEN);
		afspraakSeDto.setEerderOnderbrokenInZelfdeRonde(eerderOnderbrokenInZelfdeRonde);

		var opgeschorteBeoordelingResult = mammaBaseBeoordelingService.zoekOpgeschorteBeoordelingInRonde(afspraak.getUitnodiging().getScreeningRonde(),
			MammaBeoordelingOpschortenReden.AANVULLENDE_BEELDEN_NODIG_SE);

		afspraakSeDto.setEerdereOpschortenReden(opgeschorteBeoordelingResult.map(MammaBeoordeling::getOpschortReden).orElse(null));
		afspraakSeDto.setEerdereOpschortenRedenTekst(opgeschorteBeoordelingResult.map(MammaBeoordeling::getOpschortRedenTekst).orElse(null));

	}

	private void updateAfspraakDtoMetOpkomstTellers(MammaAfspraak afspraak, AfspraakSeDto afspraakSeDto)
	{
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		afspraakSeDto.setAantalOproepen(baseDossierService.aantalOproepen(dossier));
		afspraakSeDto.setAantalOpgekomen(baseDossierService.aantalOpgekomenSE(dossier));
	}

	private void setDefaultIdentificaties(List<AfspraakSeDto> afspraakDtos)
	{
		var clientIds = afspraakDtos.stream().map(afspraakDto -> afspraakDto.getClient().getId()).collect(Collectors.toList());
		if (!clientIds.isEmpty())
		{
			var laatsteIdentificaties = afsprakenDao.readLaatsteIdentificatieVanClienten(clientIds);
			for (var afspraakDto : afspraakDtos)
			{
				ClientIdentificatie laatsteIdentificatie = laatsteIdentificaties.get(afspraakDto.getClient().getId());
				if (laatsteIdentificatie != null)
				{
					afspraakDto.setIdentificatiesoort(laatsteIdentificatie.getSoort());
					afspraakDto.setIdentificatienummer(laatsteIdentificatie.getNummer());
				}
			}
		}
	}
}
