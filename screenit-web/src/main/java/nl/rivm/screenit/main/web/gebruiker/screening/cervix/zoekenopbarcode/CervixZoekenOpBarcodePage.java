package nl.rivm.screenit.main.web.gebruiker.screening.cervix.zoekenopbarcode;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.PostcodeColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster.CervixMonsterIdScannenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_ZOEKENOPBARCODE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class CervixZoekenOpBarcodePage extends CervixScreeningBasePage
{
	@SpringBean
	private IFobtDao iFobtDao;

	private final IModel<Client> clientModel = new SimpleHibernateModel<>();

	private final IModel<String> scanInput = new Model<>();

	private final WebMarkupContainer tabelContainer;

	public CervixZoekenOpBarcodePage()
	{
		var zoekobject = new Client();
		var persoon = new GbaPersoon();
		persoon.setGbaAdres(new BagAdres());
		zoekobject.setPersoon(persoon);

		var scannenPanel = new CervixMonsterIdScannenPanel("scanForBarcode")
		{
			@Override
			protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
			{
				if (uitnodiging != null)
				{
					clientModel.setObject(uitnodiging.getScreeningRonde().getDossier().getClient());
					tabelContainer.setVisible(true);
					replaceTabel(target);
				}
				else
				{
					warn(getString("geen.client.gevonden"));
				}
			}
		};

		add(scannenPanel);

		tabelContainer = new WebMarkupContainer("tabelContainer");
		tabelContainer.setOutputMarkupPlaceholderTag(true);
		tabelContainer.setVisible(false);
		tabelContainer.add(new WebMarkupContainer("tabel"));
		add(tabelContainer);

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		var contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.zoekenopbarcode", CervixZoekenOpBarcodePage.class));
		return contextMenuItems;
	}

	private void replaceTabel(AjaxRequestTarget target)
	{
		var columns = new ArrayList<IColumn<Client, String>>();
		columns.add(new ClientColumn<>("persoon.achternaam", ""));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "persoon.bsn", "persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "persoon"));
		columns.add(new PropertyColumn<>(Model.of("Overlijdensdatum"), "persoon.overlijdensdatum", "persoon.overlijdensdatum"));
		columns.add(new PostcodeColumn<>("persoon.gbaAdres.postcode", "persoon.gbaAdres.postcode"));
		columns.add(new PropertyColumn<>(Model.of("Huisnummer"), "persoon.gbaAdres.huisnummer", "persoon.gbaAdres.huisnummer"));

		var tabel = new ScreenitDataTable<Client, String>("tabel", columns, new SortableDataProvider<>()
		{

			@Override
			public Iterator<? extends Client> iterator(long first, long count)
			{
				return Arrays.asList(clientModel.getObject()).iterator();
			}

			@Override
			public long size()
			{
				return 1;
			}

			@Override
			public IModel<Client> model(Client object)
			{
				return new SimpleHibernateModel<>(object);
			}
		}, Model.of("cli\u00EBnt"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<Client> model)
			{
				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, Actie.INZIEN, model.getObject()))
				{
					setResponsePage(new ClientContactPage(model));
				}
				else
				{
					warn(getString("geen.recht.voor.contactscherm.client"));
				}
			}
		};
		tabelContainer.addOrReplace(tabel);
		target.add(tabelContainer);
		scanInput.setObject(null);
	}

}
