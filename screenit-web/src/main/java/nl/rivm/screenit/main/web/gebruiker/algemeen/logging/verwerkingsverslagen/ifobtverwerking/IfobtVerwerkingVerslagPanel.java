package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.ifobtverwerking;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.List;

import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportageEntry;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class IfobtVerwerkingVerslagPanel extends GenericPanel<IfobtVerwerkingBeeindigdLogEvent>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	public IfobtVerwerkingVerslagPanel(String id, IModel<IfobtVerwerkingBeeindigdLogEvent> model, final LogGebeurtenis logGebeurtenis)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("rapportage.datumVerwerking", "dd-MM-yyyy HH:mm:ss"));

		List<IfobtVerwerkingRapportageEntry> verwerkingen = model.getObject().getRapportage().getBestanden();

		if (LogGebeurtenis.IFOBT_INLEZEN_AFGEROND.equals(logGebeurtenis))
		{
			add(new Label("aantalTotaalGelezen", getAantalTotaalResultaten(verwerkingen)));
			add(new Label("aantalTotaalResultaten", getAantalTotaalIngelezen(verwerkingen)));
		}
		else
		{
			add(new Label("aantalTotaalGelezen", getAantalTotaalIngelezen(verwerkingen)));
			add(new Label("aantalTotaalResultaten", getAantalTotaalResultaten(verwerkingen)));
		}
		add(new Label("aantalTotaalBestanden", getAantalTotaalBestanden(verwerkingen)));

		add(new PropertyListView<IfobtVerwerkingRapportageEntry>("rapportage.verwerkingen", ModelUtil.listRModel(verwerkingen, false))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<IfobtVerwerkingRapportageEntry> item)
			{
				IFOBTBestand bestand = hibernateService.get(IFOBTBestand.class, item.getModelObject().getIfobtBestandId());
				if (bestand != null)
				{
					if (StringUtils.isNotBlank(bestand.getPathBestand()))
					{
						DownloadLink downloadLink = new DownloadLink("resource", new File(bestand.getPathBestand()), bestand.getNaamBestand());
						downloadLink.add(new Label("bestandsNaam", bestand.getNaamBestand()));
						item.add(downloadLink);
					}
					else
					{
						item.add(new WebMarkupContainer("resource").add(new Label("bestandsNaam", bestand.getNaamBestand())));
					}
				}
				else
				{
					item.add(new WebMarkupContainer("resource").setVisible(false));
				}
				item.add(new Label("bestandsNaam").setVisible(bestand == null));

				if (LogGebeurtenis.IFOBT_INLEZEN_AFGEROND.equals(logGebeurtenis))
				{
					item.add(new Label("aantalVerwerkingen", item.getModelObject().getAantalVerwerkingen()));
					item.add(new Label("aantalIngelezen", getAantalIngelezen(item.getModelObject())));
				}
				else
				{
					item.add(new Label("aantalVerwerkingen", getAantalIngelezen(item.getModelObject())));
					item.add(new Label("aantalIngelezen", item.getModelObject().getAantalVerwerkingen()));
				}
			}

		});

		add(new Label("melding").setEscapeModelStrings(false).setVisible(!Level.INFO.equals(model.getObject().getLevel())));
	}

	private Long getAantalTotaalIngelezen(List<IfobtVerwerkingRapportageEntry> verwerkingen)
	{
		Long totaal = Long.valueOf(0);
		if (CollectionUtils.isNotEmpty(verwerkingen))
		{
			for (IfobtVerwerkingRapportageEntry entry : verwerkingen)
			{
				totaal += getAantalIngelezen(entry);
			}
		}
		return totaal;
	}

	private Integer getAantalIngelezen(IfobtVerwerkingRapportageEntry entry)
	{
		IFOBTBestand bestand = hibernateService.get(IFOBTBestand.class, entry.getIfobtBestandId());
		if (bestand != null && !IFOBTBestandStatus.NIET_VOLLEDIG_INGELEZEN.equals(bestand.getStatus()))
		{
			return bestand.getUitslagen().size();
		}
		return 0;
	}

	private Long getAantalTotaalResultaten(List<IfobtVerwerkingRapportageEntry> verwerkingen)
	{
		Long totaal = Long.valueOf(0);
		if (CollectionUtils.isNotEmpty(verwerkingen))
		{
			for (IfobtVerwerkingRapportageEntry entry : verwerkingen)
			{
				totaal += entry.getAantalVerwerkingen();
			}
		}
		return totaal;

	}

	private Integer getAantalTotaalBestanden(List<IfobtVerwerkingRapportageEntry> verwerkingen)
	{
		if (CollectionUtils.isEmpty(verwerkingen))
		{
			return 0;
		}
		else
		{
			return verwerkingen.size();
		}
	}

}
