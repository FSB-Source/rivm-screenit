package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.IGeografischeCoordinaten;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.service.colon.VrijSlotFactory;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.ObjectUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class PlanningServiceImpl<T extends VrijSlot> implements PlanningService<T>
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningServiceImpl.class);

	@Autowired
	private RoosterDao roosterDao;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private VrijSlotFactory<T> factory;

	@Override
	public List<T> getBeschikbaarheid(LocalDate startDatum, LocalDate eindDatum, ColoscopieCentrum intakelocatie)
	{
		List<T> returnValues = new ArrayList<T>();

		RoosterListViewFilter filter = new RoosterListViewFilter();
		filter.setStartDatum(DateUtil.toUtilDate(startDatum));
		filter.setEindDatum(DateUtil.toUtilDate(eindDatum));
		filter.setStatus(RoosterItemStatus.VRIJ_TE_VERPLAATSEN);
		filter.setRekeningHoudenMetCapaciteitMeeBepaald(false);
		Iterator<RoosterItemListViewWrapper> roosterBlokken = roosterDao.getRoosterBlokken("startTime", true, -1, -1, filter, intakelocatie).iterator();
		while (roosterBlokken.hasNext())
		{
			RoosterItemListViewWrapper roosterItem = roosterBlokken.next();

			T slot = factory.createVrijSlot();

			slot.setStartTijd(roosterItem.getStartDatum());
			slot.setEindTijd(roosterItem.getEindDatum());
			slot.setKamerId(roosterItem.getKamerId());
			slot.setRoosterItemId(roosterItem.getRoosterItemId());
			slot.setMaxAantalDeelnemers(1);
			slot.setAantalDeelnemers(1);

			LOG.debug(String.format("%s (%d), %s %s - %s", roosterItem.getKamer(), roosterItem.getKamerId(), slot.getDatumAsString(), slot.getStartTijdAsString(),
				slot.getEindTijdAsString()));
			returnValues.add(slot);
		}
		return returnValues;
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, final boolean asc, long first, long count, VrijSlotZonderKamerFilter filter, Client client)
	{
		List<VrijSlotZonderKamer> vrijeSlotenZonderKamer = null;
		if (!sortProperty.equals("afstand"))
		{
			if (filter.getAfstand() == null)
			{
				vrijeSlotenZonderKamer = roosterDao.getVrijeSlotenZonderKamer(sortProperty, asc, first, count, filter);
			}
			else
			{
				vrijeSlotenZonderKamer = roosterDao.getVrijeSlotenZonderKamer(sortProperty, asc, filter);
			}
		}
		else
		{
			vrijeSlotenZonderKamer = roosterDao.getVrijeSlotenZonderKamer(filter);
		}

		voegAfstandenToe(client, vrijeSlotenZonderKamer);

		if (filter.getAfstand() != null)
		{
			vrijeSlotenZonderKamer = vrijeSlotenBinnenAfstand(vrijeSlotenZonderKamer, filter.getAfstand().doubleValue());
		}

		if (sortProperty.equals("afstand"))
		{
			vrijeSlotenZonderKamer.sort((o1, o2) ->
			{
				if (asc)
				{
					return ObjectUtils.compare(o1.getAfstand(), o2.getAfstand(), true);
				}
				else
				{
					return ObjectUtils.compare(o2.getAfstand(), o1.getAfstand(), true);
				}
			});
		}

		if (!(sortProperty.equals("afstand") || filter.getAfstand() != null))
		{
			return vrijeSlotenZonderKamer;
		}
		else
		{
			return vrijeSlotenZonderKamer.subList((int) first, (int) (first + count));
		}
	}

	@Override
	public long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter, Client client)
	{
		if (filter.getAfstand() == null)
		{
			return roosterDao.getVrijeSlotenZonderKamerCount(filter);
		}
		else
		{
			List<VrijSlotZonderKamer> vrijeSlotenZonderKamer = roosterDao.getVrijeSlotenZonderKamer(filter);
			voegAfstandenToe(client, vrijeSlotenZonderKamer);

			List<VrijSlotZonderKamer> vrijeSlotenBinnenAfstand = vrijeSlotenBinnenAfstand(vrijeSlotenZonderKamer, filter.getAfstand().doubleValue());
			return Long.valueOf(vrijeSlotenBinnenAfstand.size());
		}
	}

	private void voegAfstandenToe(Client client, List<VrijSlotZonderKamer> vrijeSlotenZonderKamer)
	{
		GbaPersoon persoon = client.getPersoon();
		PersoonCoordinaten persoonCoordinaten = coordinatenService.getAdresEnTijdelijkAdresCoordinatenVanPersoon(persoon);

		Map<Long, Double> adresAfstandenMap = new HashMap<>();
		Map<Long, Double> tijdelijkAdresAfstandenMap = new HashMap<>();
		for (VrijSlotZonderKamer vrijSlotZonderKamer : vrijeSlotenZonderKamer)
		{
			if (AdresUtil.isTijdelijkAdres(persoon, DateUtil.toLocalDate(vrijSlotZonderKamer.getStartTijd())) && persoonCoordinaten.vanTijdelijkAdres != null)
			{
				if (!tijdelijkAdresAfstandenMap.containsKey(vrijSlotZonderKamer.getIntakeLocatieId()))
				{
					ColoscopieCentrum intakeLocatie = hibernateService.load(ColoscopieCentrum.class, vrijSlotZonderKamer.getIntakeLocatieId());
					IGeografischeCoordinaten intakeLocatieCoordinaten = intakeLocatie.getPostcodeCoordinaten();
					if (intakeLocatieCoordinaten != null)
					{
						Double afstand = BigDecimalUtil.berekenDistance(persoonCoordinaten.vanTijdelijkAdres, intakeLocatieCoordinaten);
						tijdelijkAdresAfstandenMap.put(vrijSlotZonderKamer.getIntakeLocatieId(), afstand);
						vrijSlotZonderKamer.setAfstand(afstand);
					}
				}
				else
				{
					vrijSlotZonderKamer.setAfstand(tijdelijkAdresAfstandenMap.get(vrijSlotZonderKamer.getIntakeLocatieId()));
				}
			}
			else
			{
				if (!adresAfstandenMap.containsKey(vrijSlotZonderKamer.getIntakeLocatieId()))
				{
					ColoscopieCentrum intakeLocatie = hibernateService.load(ColoscopieCentrum.class, vrijSlotZonderKamer.getIntakeLocatieId());
					IGeografischeCoordinaten intakeLocatieCoordinaten = intakeLocatie.getPostcodeCoordinaten();
					if (intakeLocatieCoordinaten != null && persoonCoordinaten.vanAdres != null)
					{
						Double afstand = BigDecimalUtil.berekenDistance(persoonCoordinaten.vanAdres, intakeLocatieCoordinaten);
						adresAfstandenMap.put(vrijSlotZonderKamer.getIntakeLocatieId(), afstand);
						vrijSlotZonderKamer.setAfstand(afstand);
					}
				}
				else
				{
					vrijSlotZonderKamer.setAfstand(adresAfstandenMap.get(vrijSlotZonderKamer.getIntakeLocatieId()));
				}
			}
		}
	}

	private List<VrijSlotZonderKamer> vrijeSlotenBinnenAfstand(List<VrijSlotZonderKamer> vrijeSlotenZonderKamer, Double afstand)
	{
		List<VrijSlotZonderKamer> vrijeSlotenBinnenAfstand = new ArrayList<>();
		for (VrijSlotZonderKamer vrijSlotZonderKamer : vrijeSlotenZonderKamer)
		{
			if (vrijSlotZonderKamer.getAfstand() != null && vrijSlotZonderKamer.getAfstand() <= afstand)
			{
				vrijeSlotenBinnenAfstand.add(vrijSlotZonderKamer);
			}
		}
		return vrijeSlotenBinnenAfstand;
	}

	@Override
	public Kamer getBeschikbareKamer(Date startTijd, Long intakeLocatieId)
	{
		List<Kamer> kamers = roosterDao.getKamers(startTijd, intakeLocatieId);
		if (kamers.size() > 0)
		{
			return kamers.get(0);
		}
		return null;
	}
}
