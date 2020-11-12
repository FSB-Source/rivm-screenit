package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaPalgaDao;
import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaPalgaDaoImpl extends AbstractAutowiredDao implements MammaPalgaDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private FileService fileService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public long getPatid3MatchCount(GbaPersoon persoon)
	{
		Criteria crit = getSession().createCriteria(Client.class, "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("client.mammaDossier", "mammaDossier");
		crit.add(Restrictions.eq("persoon.geboortedatum", persoon.getGeboortedatum()));
		crit.add(Restrictions.like("persoon.achternaam", getWildcardAchternaam(persoon.getAchternaam()), MatchMode.EXACT));
		crit.setProjection(Projections.rowCount());
		return (long) crit.uniqueResult();
	}

	private String getWildcardAchternaam(String achternaam)
	{
		StringBuilder wildcardAchternaam = new StringBuilder();
		char[] chars = achternaam.toCharArray();
		for (int i = 0; i < chars.length; i++)
		{
			if (!isGenormaliseerdKarakter(chars[i], i != chars.length - 1 ? chars[i + 1] : ' '))
			{
				wildcardAchternaam.append(chars[i]);
			}
			else
			{
				wildcardAchternaam.append("_");
			}
		}
		return wildcardAchternaam.toString();
	}

	private boolean isGenormaliseerdKarakter(char karakter, char opvolgendKarakter)
	{
		karakter = Character.toLowerCase(karakter);
		opvolgendKarakter = Character.toLowerCase(opvolgendKarakter);
		return !StringUtil.isAlfabetKarakter(karakter) || karakter == 'y' || (karakter == 'i' && opvolgendKarakter == 'j');
	}

	@Override
	public List<Long> getClientenVoorPalga()
	{
		Criteria crit = getSession().createCriteria(Client.class, "client");
		crit.createAlias("client.mammaDossier", "mammaDossier");
		crit.createAlias("mammaDossier.laatsteBeoordelingMetUitslag", "laatsteBeoordelingMetUitslag");
		if (preferenceService.getBoolean(PreferenceKey.MAMMA_PALGA_EXPORT_ALLEEN_VERWEZEN.name()))
		{
			crit.add(Restrictions.eq("laatsteBeoordelingMetUitslag.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG));
		}

		crit.add(Restrictions.ne("client.gbaStatus", GbaStatus.AFGEVOERD));
		crit.add(Restrictions.ne("client.gbaStatus", GbaStatus.BEZWAAR));

		addClientBezwaarRestrictions(crit);

		crit.add(
			DateRestrictions.ge("laatsteBeoordelingMetUitslag.statusDatum", DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusMonths(30))));
		crit.setProjection(Projections.id());

		return crit.list();
	}

	private void addClientBezwaarRestrictions(Criteria crit)
	{
		DetachedCriteria subcriteria = DetachedCriteria.forClass(Bezwaar.class);
		subcriteria.add(Restrictions.in("type", Arrays.asList(BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, BezwaarType.GEEN_KWALITEITSWAARBORGING)));
		subcriteria.add(Restrictions.eq("bevolkingsonderzoek", Bevolkingsonderzoek.MAMMA));

		subcriteria.setProjection(Projections.property("bezwaarMoment"));

		crit.add(Restrictions.or(
			Subqueries.propertyNotIn("client.laatstVoltooideBezwaarMoment", subcriteria),
			Restrictions.isNull("client.laatstVoltooideBezwaarMoment")));
	}

	@Override
	public boolean heeftBezwaar(Client client)
	{
		Criteria crit = getSession().createCriteria(Client.class, "client");
		crit.add(Restrictions.eq("client.id", client.getId()));
		addClientBezwaarRestrictions(crit);

		return crit.list().size() != 1;
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void deleteExports()
	{
		List<UploadDocument> exports = getExports();
		for (UploadDocument export : exports)
		{
			fileService.delete(export, true);
		}
	}

	@Override
	public UploadDocument getExport()
	{
		Criteria crit = getExportCriteria();
		crit.addOrder(Order.desc("naam"));
		return crit.list().size() != 0 ? (UploadDocument) crit.list().get(0) : null;
	}

	private List<UploadDocument> getExports()
	{
		Criteria crit = getExportCriteria();
		return crit.list();
	}

	private Criteria getExportCriteria()
	{
		Criteria crit = getSession().createCriteria(UploadDocument.class);
		crit.add(Restrictions.eq("contentType", "application/zip"));
		crit.add(Restrictions.like("naam", "CHTRDS", MatchMode.START));
		crit.add(Restrictions.like("naam", ".zip", MatchMode.END));
		crit.add(Restrictions.like("path", FileStoreLocation.MAMMA_PALGA_CSV_EXPORT.getPath().replaceAll("\\\\", "\\\\\\\\"), MatchMode.START));
		return crit;
	}

	@Override
	public UploadDocument getImport()
	{
		Criteria crit = getImportCriteria();
		return crit.list().size() != 0 ? (UploadDocument) crit.list().get(0) : null;
	}

	private List<UploadDocument> getImports()
	{
		Criteria crit = getImportCriteria();
		return crit.list();
	}

	private Criteria getImportCriteria()
	{
		Criteria crit = getSession().createCriteria(UploadDocument.class);
		crit.add(Restrictions.in("contentType", "application/octet-stream", "application/vnd.ms-excel"));
		crit.add(Restrictions.like("naam", ".csv", MatchMode.END));
		crit.add(Restrictions.like("path", FileStoreLocation.MAMMA_PALGA_CSV_IMPORT.getPath().replaceAll("\\\\", "\\\\\\\\"), MatchMode.START));
		crit.addOrder(Order.desc("naam"));
		return crit;
	}

	@Override
	public void deleteImports()
	{
		List<UploadDocument> imports = getImports();
		for (UploadDocument importDoc : imports)
		{
			fileService.delete(importDoc, true);
		}
	}

}
