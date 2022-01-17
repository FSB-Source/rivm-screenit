package nl.rivm.screenit.main.web.gebruiker.screening.colon.zoekenopbarcode;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ZoekIfobtMetBarcodePanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.util.postcode.PostcodeFormatter;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_ZOEKENOPBARCODE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ZoekenOpBarcodePage extends ColonScreeningBasePage
{

	private static final long serialVersionUID = 1L;

	private final IModel<Client> clientModel = new SimpleHibernateModel<>();

	private final IModel<String> scanInput = new Model<String>();

	private final WebMarkupContainer tabelContainer;

	public ZoekenOpBarcodePage()
	{
		Client zoekobject = new Client();
		GbaPersoon persoon = new GbaPersoon();
		persoon.setGbaAdres(new BagAdres());
		zoekobject.setPersoon(persoon);

		ZoekIfobtMetBarcodePanel scanForIfobttest = new ZoekIfobtMetBarcodePanel("scanForIfobttest")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void ifobtFound(IFOBTTest ifobtTest, AjaxRequestTarget target)
			{
				super.ifobtFound(ifobtTest, target);
				if (ifobtTest != null)
				{
					clientModel.setObject(ifobtTest.getColonScreeningRonde().getDossier().getClient());
					tabelContainer.setVisible(true);
					replaceTabel(target);
				}
				else
				{
					info("Geen cli\u00EBnt gevonden");
				}
			}

		};
		add(scanForIfobttest);

		tabelContainer = new WebMarkupContainer("tabelContainer");
		tabelContainer.setOutputMarkupPlaceholderTag(true);
		tabelContainer.setVisible(false);
		tabelContainer.add(new WebMarkupContainer("tabel"));
		add(tabelContainer);

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.zoekenopbarcode", ZoekenOpBarcodePage.class));
		return contextMenuItems;
	}

	private void replaceTabel(AjaxRequestTarget target)
	{
		List<IColumn<Client, String>> columns = new ArrayList<>();
		columns.add(new ClientColumn<>("persoon.achternaam", ""));
		columns.add(new PropertyColumn<Client, String>(Model.of("Bsn"), "persoon.bsn", "persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "persoon"));

		columns.add(new PropertyColumn<Client, String>(Model.of("Overlijdensdatum"), "persoon.overlijdensdatum", "persoon.overlijdensdatum"));

		columns.add(new PropertyColumn<Client, String>(Model.of("Postcode"), "persoon.gbaAdres.postcode", "persoon.gbaAdres.postcode")
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<Client> rowModel)
			{
				return new Model(PostcodeFormatter.formatPostcode((String) super.getDataModel(rowModel).getObject(), true));
			}

		});
		columns.add(new PropertyColumn<Client, String>(Model.of("Huisnummer"), "persoon.gbaAdres.huisnummer", "persoon.gbaAdres.huisnummer"));

		final ScreenitDataTable<Client, String> tabel = new ScreenitDataTable<Client, String>("tabel", columns, new SortableDataProvider<Client, String>()
		{

			private static final long serialVersionUID = 1L;

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
		}, Model.of("client(en)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Client> model)
			{
				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_GEGEVENS, Actie.INZIEN, model.getObject()))
				{
					setResponsePage(new ClientInzienPage(model));
				}
			}
		};
		tabelContainer.addOrReplace(tabel);
		target.add(tabelContainer);
		scanInput.setObject(null);
	}

}
