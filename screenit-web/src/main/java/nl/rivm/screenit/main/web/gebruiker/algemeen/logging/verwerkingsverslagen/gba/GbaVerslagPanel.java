
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.gba;

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
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.gba.GbaFile;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingEntry;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.request.resource.ResourceStreamResource;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.FileResourceStream;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class GbaVerslagPanel extends GenericPanel<GbaVerwerkingsLog>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private FileService fileService;

	@SpringBean(name = "voFileStorePath")
	private String voFileStorePath;

	public GbaVerslagPanel(String id, IModel<GbaVerwerkingsLog> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("aantalNieuweBurgers", getAantalNieuweBurgers(model)));
		add(new Label("aantalBijgewerkteBugers", getAantalBijgewerkteBurgers(model)));
		add(new Label("aantalNieuweColonDossiers"));
		add(new Label("aantalNieuweCervixDossiers"));
		add(new Label("aantalNieuweMammaDossiers"));
		add(new PropertyListView<GbaFile>("bestanden")
		{
			@Override
			protected void populateItem(ListItem<GbaFile> item)
			{
				var bestandLocatie = voFileStorePath + item.getModelObject().getPath();

				var resource = new ResourceStreamResource(new FileResourceStream(fileService.load(bestandLocatie)))
					.setContentDisposition(ContentDisposition.ATTACHMENT)
					.setCacheDuration(Duration.ZERO)
					.setFileName(System.currentTimeMillis() + "-" + item.getModelObject().getNaam());
				ResourceLink<Void> resourceLink = new ResourceLink<>("resource", resource);
				resourceLink.add(new Label("naam"));
				item.add(resourceLink);
			}
		});

		List<GbaFoutRegel> fouten = new ArrayList<>();
		if (model.getObject() != null)
		{
			fouten = model.getObject().getFouten();
		}
		add(new PropertyListView<>("fouten", filterFouten(fouten))
		{
			@Override
			protected void populateItem(ListItem<GbaFoutRegel> item)
			{
				item.add(new Label("fout"));
			}
		});
	}

	private String getAantalNieuweBurgers(IModel<GbaVerwerkingsLog> model)
	{
		if (model.getObject() == null)
		{
			return null;
		}

		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_GBA_VERWERKING_VERSLAG);
		if (toegangLevel == ToegangLevel.LANDELIJK)
		{
			return model.getObject().getAantalNieuweBurgers().toString();
		}
		else if (toegangLevel == ToegangLevel.REGIO && ScreenitSession.get().getScreeningOrganisatie() != null)
		{
			for (GbaVerwerkingEntry entry : model.getObject().getEntries())
			{
				if (entry.getScreeningOrganisatie().equals(ScreenitSession.get().getScreeningOrganisatie().getId()))
				{
					return entry.getAantalNieuweBurgers().toString();
				}
			}
		}
		return null;
	}

	private String getAantalBijgewerkteBurgers(IModel<GbaVerwerkingsLog> model)
	{
		if (model.getObject() == null)
		{
			return null;
		}

		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_GBA_VERWERKING_VERSLAG);
		if (toegangLevel == ToegangLevel.LANDELIJK)
		{
			return model.getObject().getAantalBijgewerkteBugers().toString();
		}
		else if (toegangLevel == ToegangLevel.REGIO && ScreenitSession.get().getScreeningOrganisatie() != null)
		{
			for (GbaVerwerkingEntry entry : model.getObject().getEntries())
			{
				if (entry.getScreeningOrganisatie().equals(ScreenitSession.get().getScreeningOrganisatie().getId()))
				{
					return entry.getAantalBijgewerkteBugers().toString();
				}
			}
		}
		return null;
	}

	private IModel<List<GbaFoutRegel>> filterFouten(List<GbaFoutRegel> fouten)
	{
		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_GBA_VERWERKING_VERSLAG);
		if (toegangLevel == ToegangLevel.LANDELIJK)
		{
			return new SimpleListHibernateModel<>(fouten);
		}
		else if (toegangLevel == ToegangLevel.REGIO && ScreenitSession.get().getScreeningOrganisatie() != null)
		{
			List<GbaFoutRegel> gefilterd = new ArrayList<>();
			for (GbaFoutRegel foutRegel : fouten)
			{
				if (foutRegel.getClient() == null)
				{
					gefilterd.add(foutRegel);
				}
				else
				{
					Client client = hibernateService.load(Client.class, foutRegel.getClient());
					if (ScreenitSession.get().getScreeningOrganisatie().equals(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie()))
					{
						gefilterd.add(foutRegel);
					}
				}
			}
			return new SimpleListHibernateModel<>(gefilterd);
		}
		return new ListModel<GbaFoutRegel>(Collections.EMPTY_LIST);
	}
}
