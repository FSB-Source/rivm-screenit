package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.GemeenteService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.HibernateListDataProvider;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	level = ToegangLevel.REGIO,
	recht = Recht.GEBRUIKER_BEHEER_GEBIEDEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class GemeenteGegevens extends GebiedenBeheerPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private LogService logService;

	@SpringBean
	private GemeenteService gemeenteService;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	private WebMarkupContainer refreshContainer;

	private final BootstrapDialog dialog;

	public GemeenteGegevens(IModel<Gemeente> model)
	{
		setDefaultModel(model);
		dialog = new BootstrapDialog("dialog");
		add(dialog);
		addOrReplaceMarkup(null);
	}

	private void addOrReplaceMarkup(AjaxRequestTarget target)
	{
		WebMarkupContainer newRefreshContainer = new WebMarkupContainer("refreshContainer");
		newRefreshContainer.setOutputMarkupId(true);

		if (this.refreshContainer == null)
		{
			this.refreshContainer = newRefreshContainer;
			add(refreshContainer);
		}
		else
		{
			this.refreshContainer.replaceWith(newRefreshContainer);
			this.refreshContainer = newRefreshContainer;
		}
		if (target != null)
		{
			target.add(refreshContainer);
		}

		IModel<Gemeente> model = (IModel<Gemeente>) getDefaultModel();
		Gemeente gemeente = model.getObject();

		Boolean gesplitsOpPostcode = gemeenteService.getGesplitsOpPostcode(gemeente);

		refreshContainer.add(new GemeentePaspoortPanel("paspoort", model));

		AjaxLink<Gemeente> allesVerwijderen = new ConfirmingIndicatingAjaxLink<Gemeente>("allesVerwijderen", model, dialog, "question.remove.all.gebieden")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Gemeente gemeente = (Gemeente) GemeenteGegevens.this.getDefaultModelObject();
				if (gemeenteService.magAlleGebiedenVerwijderen(gemeente))
				{
					gemeenteService.verwijderAlleGebieden(gemeente);
					GemeenteGegevens.this.setDefaultModelObject(gemeente);
					addOrReplaceMarkup(target);
				}
				else
				{
					error(getString("gebieden.niet.te.verwijderen"));
				}
			}

		};
		allesVerwijderen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN, Actie.VERWIJDEREN) && gemeente.getUitnodigingsGebieden().size() > 1);
		refreshContainer.add(allesVerwijderen);

		WebMarkupContainer opsplitsen = new WebMarkupContainer("opsplitsen");
		opsplitsen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN, Actie.TOEVOEGEN));
		refreshContainer.add(opsplitsen);
		opsplitsen.add(new IndicatingAjaxLink<Gemeente>("woonplaats")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UitnodigingsGebied nieuweUitnodigingsGebied = new UitnodigingsGebied();
				nieuweUitnodigingsGebied.setGemeente((Gemeente) GemeenteGegevens.this.getDefaultModelObject());
				nieuweUitnodigingsGebied.setNaam("<Kies woonplaats>");
				nieuweUitnodigingsGebied.setWoonplaats("<Kies woonplaats>");
				setResponsePage(new GebiedGegevens(ModelUtil.cModel(nieuweUitnodigingsGebied), Boolean.FALSE));
			}

		}.setVisible(!Boolean.TRUE.equals(gesplitsOpPostcode)));
		opsplitsen.add(new IndicatingAjaxLink<Gemeente>("postcode")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UitnodigingsGebied nieuweUitnodigingsGebied = new UitnodigingsGebied();
				nieuweUitnodigingsGebied.setGemeente((Gemeente) GemeenteGegevens.this.getDefaultModelObject());
				nieuweUitnodigingsGebied.setNaam("<Kies postcodegebied>");
				nieuweUitnodigingsGebied.setPostcodeGebied(new PostcodeGebied());
				setResponsePage(new GebiedGegevens(ModelUtil.cModel(nieuweUitnodigingsGebied), Boolean.TRUE));
			}

		}.setVisible(!Boolean.FALSE.equals(gesplitsOpPostcode)));

		List<IColumn<UitnodigingsGebied, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<UitnodigingsGebied, String>(Model.of("Naam gebied"), "naam"));
		columns.add(new AbstractColumn<UitnodigingsGebied, String>(Model.of("Gekoppelde intakelocaties"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<UitnodigingsGebied>> cellItem, String componentId, IModel<UitnodigingsGebied> rowModel)
			{
				Set<ColoscopieCentrum> centra = new HashSet<ColoscopieCentrum>();
				for (ColoscopieCentrumColonCapaciteitVerdeling verdeling : rowModel.getObject().getVerdeling())
				{
					centra.add(verdeling.getColoscopieCentrum());
				}
				cellItem.add(new Label(componentId, Model.of(centra.size())));
			}

		});

		ScreenitDataTable<UitnodigingsGebied, String> dataTabel = new ScreenitDataTable<UitnodigingsGebied, String>("gebieden", columns,
			new HibernateListDataProvider<UitnodigingsGebied, String>(((Gemeente) getDefaultModelObject()).getUitnodigingsGebieden()), new Model<>("gebieden"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<UitnodigingsGebied> model)
			{
				setResponsePage(new GebiedGegevens(ModelUtil.cModel(model.getObject())));
			}

		};
		refreshContainer.add(dataTabel);
	}
}
