package nl.rivm.screenit.main.web.gebruiker.algemeen.nieuws;

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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.NieuwsService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.NIEUWS_WIJZIGEN, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON,
	Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class BeheerNieuwsPage extends AlgemeenPage
{
	@SpringBean
	private NieuwsService nieuwsService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		List<NieuwsItem> nieuwsItems = nieuwsService.getNieuwsItems(true);

		IModel<NieuwsItem> nieuwsItemIModel;
		if (nieuwsItems.isEmpty())
		{
			nieuwsItemIModel = nieuwNieuwsItem();
		}
		else
		{

			nieuwsItemIModel = ModelUtil.cModel(nieuwsItems.get(0));
		}

		add(new EditForm("form", nieuwsItemIModel));
	}

	private IModel<NieuwsItem> nieuwNieuwsItem()
	{
		NieuwsItem nieuwsItem = new NieuwsItem();
		nieuwsItem.setGemaaktDoor(((ScreenitSession) getSession()).getLoggedInInstellingGebruiker());
		nieuwsItem.setGemaakt(currentDateSupplier.getDate());
		return ModelUtil.cModel(nieuwsItem);
	}

	private class EditForm extends ScreenitForm<NieuwsItem>
	{
		private Label gewijzigd;

		public EditForm(String id, IModel<NieuwsItem> model)
		{
			super(id, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();

			NieuwsItem nieuwsItem = getModelObject();

			add(ComponentHelper.addTextField(this, "titel", true, 255, false));
			add(ComponentHelper.newTextArea("tekst", 4096).setRequired(true));

			add(ComponentHelper.addTextField(this, "publicerenVanaf", true, 20, Date.class, false));
			add(new Label("gemaakt", opDoorTekst(nieuwsItem.getGemaakt(), nieuwsItem.getGemaaktDoor())));
			String gewijzigdTekst = "";
			if (nieuwsItem.getGewijzigd() != null)
			{
				gewijzigdTekst = opDoorTekst(nieuwsItem.getGewijzigd(), nieuwsItem.getGewijzigdDoor());
			}
			gewijzigd = new Label("gewijzigd", Model.of(gewijzigdTekst));
			add(gewijzigd.setOutputMarkupId(true));

			add(new AjaxSubmitLink("opslaan")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					NieuwsItem formNieuwsItem = (NieuwsItem) getForm().getModelObject();

					if (DateUtil.startDag(formNieuwsItem.getPublicerenVanaf()).before(currentDateSupplier.getDateMidnight()))
					{
						error("Publiceren in het verleden is niet mogelijk.");
					}
					else if (formNieuwsItem.getPublicerenTot() != null && formNieuwsItem.getPublicerenVanaf().after(formNieuwsItem.getPublicerenTot()))
					{
						error("Publiceren vanaf ligt na publiceren tot.");
					}
					else
					{
						formNieuwsItem.setGewijzigdDoor(((ScreenitSession) getSession()).getLoggedInInstellingGebruiker());
						formNieuwsItem.setGewijzigd(currentDateSupplier.getDate());

						String gewijzigdTekst = "";
						if (formNieuwsItem.getGewijzigd() != null)
						{
							gewijzigdTekst = opDoorTekst(formNieuwsItem.getGewijzigd(), formNieuwsItem.getGewijzigdDoor());
						}
						Label newGewijzigd = new Label("gewijzigd", Model.of(gewijzigdTekst));
						newGewijzigd.setOutputMarkupId(true);
						gewijzigd.replaceWith(newGewijzigd);
						gewijzigd = newGewijzigd;

						target.add(gewijzigd);

						hibernateService.saveOrUpdate(formNieuwsItem);

						info("Nieuws opgeslagen");
					}
				}
			});
		}

		private String opDoorTekst(Date date, InstellingGebruiker instellingGebruiker)
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			return "op " + format.format(date) + " door " + instellingGebruiker.getMedewerker().getNaamVolledig();
		}
	}
}
