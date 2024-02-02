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
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dao.MammaOnderzoekenDao;
import nl.rivm.screenit.mamma.se.dao.ZorginstellingenDao;
import nl.rivm.screenit.mamma.se.dto.ZorginstellingDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdRedenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.SignalerenOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.OnderzoekAfrondenService;
import nl.rivm.screenit.mamma.se.service.OnderzoekService;
import nl.rivm.screenit.mamma.se.service.dtomapper.ZorginstellingDtoMapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class OnderzoekServiceImpl implements OnderzoekService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ZorginstellingenDao zorginstellingenDao;

	@Autowired
	private MammaOnderzoekenDao onderzoekDao;

	@Autowired
	private OnderzoekAfrondenService onderzoekAfrondenService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Override
	public void opslaan(OnderzoekOpslaanDto action, InstellingGebruiker gebruiker)
	{
		MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), gebruiker);
		OnderzoekSeDto onderzoekSeDto = action.getOnderzoek();

		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		onderzoek.setEerderMammogramZorginstelling(
			onderzoekSeDto.getEerderMammogramZorginstellingId() == null ? null : hibernateService.get(ZorgInstelling.class, onderzoekSeDto.getEerderMammogramZorginstellingId()));
		onderzoek.setEerderMammogramJaartal(onderzoekSeDto.getEerderMammogramJaartal());
		onderzoek.setSuboptimaleInsteltechniek(onderzoekSeDto.getSuboptimaleInsteltechniek());
		onderzoek.setRedenFotobespreking(onderzoekSeDto.getRedenFotobespreking());
		onderzoek.setExtraMedewerker(
			onderzoekSeDto.getExtraMedewerkerId() == null ? null : hibernateService.get(InstellingGebruiker.class, onderzoekSeDto.getExtraMedewerkerId()));
		onderzoek.setOpmerkingMbber(onderzoekSeDto.getOpmerkingMbber());
		onderzoek.setOpmerkingVoorRadioloog(onderzoekSeDto.getOpmerkingVoorRadioloog());
		onderzoek.setOperatieRechts(onderzoekSeDto.isOperatieRechts());
		onderzoek.setOperatieLinks(onderzoekSeDto.isOperatieLinks());
		onderzoek.setAmputatie(onderzoekSeDto.getAmputatie());
		onderzoek.setAanvullendeInformatieOperatie(onderzoekSeDto.getAanvullendeInformatieOperatie());
		onderzoek.setStatus(onderzoekSeDto.getStatus());
		onderzoek.setOnderzoekType(onderzoekSeDto.getOnderzoekType() != null ? onderzoekSeDto.getOnderzoekType() : MammaOnderzoekType.MAMMOGRAFIE);

		onderzoek.setOnvolledigOnderzoek(onderzoekSeDto.getOnvolledigOnderzoek());
		onderzoek.setOnderbrokenOnderzoek(onderzoekSeDto.getOnderbrokenOnderzoek());
		onderzoek.setExtraFotosRedenen(onderzoekSeDto.getExtraFotosRedenen());

		onderzoek.setAdviesHuisarts(onderzoekSeDto.getAdviesHuisarts());

		hibernateService.saveOrUpdate(onderzoek);
	}

	@Override
	public void signalerenOpslaan(SignalerenOpslaanDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		SignalerenSeDto signalering = action.getSignaleren();
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		onderzoekAfrondenService.maakSignalering(instellingGebruiker, onderzoek, signalering, transactieDatumTijd);

	}

	@Override
	public void maakDubbeleTijd(MaakDubbeleTijdDto action, InstellingGebruiker account, LocalDateTime transactieDatumTijd)
	{
		MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), account);
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();

		if (moetDoelgroepUpdaten(action, dossier))
		{
			dossier.setDoelgroep(action.isDubbeleTijd() ? MammaDoelgroep.DUBBELE_TIJD : MammaDoelgroep.REGULIER);
			maakLoggebeurtenisDoelgroepGewijzigd(afspraak, account, transactieDatumTijd);
			baseKansberekeningService.dossierEventHerzien(dossier);
		}
	}

	@Override
	public void maakDubbeleTijdReden(MaakDubbeleTijdRedenDto action, InstellingGebruiker account)
	{
		MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), account);
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();

		dossier.setDubbeleTijdReden(action.getDubbeleTijdReden());
	}

	@Override
	public Map<Long, Integer> getOnderzochtByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekDao.readOnderzochtVanSeOpWerkdag(datum, seCode);
	}

	@Override
	public Map<Long, Integer> getAfgerondByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekDao.readAfgerondVanSeOpWerkdag(datum, seCode);
	}

	@Override
	public Map<Long, Integer> getOnderbrokenByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekDao.readOnderbrokenVanSeOpWerkdag(datum, seCode);
	}

	@Override
	public Map<Long, Integer> getOnvolledigByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekDao.readOnvolledigVanSeOpWerkdag(datum, seCode);
	}

	@Override
	public Map<Long, Integer> getAfwijkingenByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekDao.readAfwijkingenVanSeOpWerkdag(datum, seCode);
	}

	@Override
	public int getAantalOnderzoekenMetBeelden(Date datum, String seCode)
	{
		Long count = onderzoekDao.readAantalOnderzoekenMetBeelden(datum, seCode);
		return count.intValue();
	}

	@Override
	public int getAantalOnderzoekenMetBeeldenBeschikbaarInIms(Date datum, String seCode)
	{
		Long count = onderzoekDao.readAantalOnderzoekenMetBeeldenBeschikbaarInIms(datum, seCode);
		return count.intValue();
	}

	@Override
	public int getAantalDoorgevoerdVanDag(Date datum, String seCode)
	{
		Long count = onderzoekDao.readDoorgevoerdeOnderzoekenVanSeOpWerkdag(datum, seCode);
		return count.intValue();
	}

	@Override
	public List<ZorginstellingDto> getBKZorginstellingen()
	{
		ZorginstellingDtoMapper mapper = new ZorginstellingDtoMapper();
		return zorginstellingenDao.getBKZorginstellingen().stream().map(mapper::createZorginstellingDto).collect(Collectors.toList());
	}

	private void maakLoggebeurtenisDoelgroepGewijzigd(MammaAfspraak afspraak, InstellingGebruiker account, LocalDateTime transactieDatumTijd)
	{
		ClientContact contact = new ClientContact();
		final Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		contact.setClient(client);
		contact.setInstellingGebruiker(account);
		contact.setDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime()));

		ClientContactActie actie = new ClientContactActie();
		actie.setType(ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN);
		actie.setContact(contact);
		hibernateService.saveOrUpdateAll(contact, actie);

		String diffFieldToLatestVersion = EntityAuditUtil.getDiffFieldsToLatestVersion(afspraak.getUitnodiging().getScreeningRonde().getDossier(),
			hibernateService.getHibernateSession(), "doelgroep");
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_DOELGROEP_GEWIJZIGD, afspraak.getStandplaatsPeriode().getScreeningsEenheid(), account, client, diffFieldToLatestVersion,
			transactieDatumTijd, Bevolkingsonderzoek.MAMMA);
	}

	private boolean moetDoelgroepUpdaten(MaakDubbeleTijdDto action, MammaDossier dossier)
	{
		return (action.isDubbeleTijd() && dossier.getDoelgroep().equals(MammaDoelgroep.REGULIER)) ||
			(!action.isDubbeleTijd() && dossier.getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD));
	}
}
