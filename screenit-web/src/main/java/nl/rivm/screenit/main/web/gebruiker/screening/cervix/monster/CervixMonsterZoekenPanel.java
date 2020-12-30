package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TablePerClassHibernateObject;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixMonsterZoekenPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private CervixMonsterDao monsterDao;

	private CervixUitnodigingenPanel uitnodigingenPanel;

	private IModel<String> monsterIdModel = Model.of("");

	private TextField<String> monsterIdField;

	private IModel<String> bsnModel = Model.of("");

	private TextField<String> bsnField;

	private IModel<Date> geboortedatumModel = Model.of();

	private DatePicker<Date> geboortedatumField;

	public CervixMonsterZoekenPanel(String id)
	{
		super(id);
		Form form = new Form<>("form");
		add(form);

		monsterIdField = new TextField<>("monsterId", monsterIdModel);
		form.add(monsterIdField.setRequired(false).setOutputMarkupId(true).add(new AttributeModifier("autofocus", "")));

		bsnField = new TextField<>("bsn", bsnModel);
		bsnField.add(new BSNValidator());
		bsnField.setRequired(false);
		bsnField.setOutputMarkupId(true);
		form.add(bsnField);

		geboortedatumField = ComponentHelper.newDatePicker("geboortedatum", geboortedatumModel);
		form.add(geboortedatumField.setRequired(false).setOutputMarkupId(true));

		uitnodigingenPanel = new CervixUitnodigingenPanel("uitnodigingenPanel")
		{

			@Override
			protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
			{
				CervixMonsterZoekenPanel.this.setUitnodiging(target, uitnodiging);
			}
		};
		add(uitnodigingenPanel);

		AjaxSubmitLink zoekenButton = new AjaxSubmitLink("zoeken")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				final String monsterId = monsterIdModel.getObject();
				final String bsn = bsnModel.getObject();
				final Date geboortedatum = geboortedatumModel.getObject();

				final int aantalIngevuldeVelden = (StringUtils.isBlank(monsterId) ? 0 : 1) + (StringUtils.isBlank(bsn) ? 0 : 1) + (geboortedatum == null ? 0 : 1);
				if (aantalIngevuldeVelden < 2)
				{
					error(getString("minstens.2.velden"));
					return;
				}

				if (StringUtils.isNotBlank(monsterId))
				{
					if (monsterId.charAt(0) == 'Z')
					{
						final String monsterNummer = monsterId.substring(1);
						if (!StringUtils.isNumeric(monsterNummer))
						{
							error(getString("nummer.invullen.na.z"));
							return;
						}
					}
					else if (!StringUtils.isNumeric(monsterId))
					{
						error(getString("nummer.invullen"));
						return;
					}
				}
				if (StringUtils.isNotBlank(bsn) && !StringUtils.isNumeric(bsn))
				{
					error(getString("bsn.geen.nummer"));
					return;
				}

				if (StringUtils.isNotBlank(monsterId))
				{
					CervixMonster monster = monsterDao.getMonsterByMonsterId(monsterId);
					if (monster == null)
					{
						error(getString("geen.met.monster.id"));
						return;
					}
					final Client client = monster.getUitnodiging().getScreeningRonde().getDossier().getClient();
					final GbaPersoon persoon = client.getPersoon();
					if (StringUtils.isNotBlank(bsn) && !bsn.equals(persoon.getBsn()))
					{
						error(getString("geen.met.monster.id.en.bsn"));
						return;
					}
					if (geboortedatum != null && !geboortedatum.equals(persoon.getGeboortedatum()))
					{
						error(getString("geen.met.monster.id.en.geboortedatum"));
						return;
					}
					toonUitnodigingen(target, client, Arrays.asList(monster.getUitnodiging()));
					return;
				}

				Client client = clientService.getClientByBsn(bsn);
				if (client == null)
				{
					error(getString("bsn.onbekend"));
					return;
				}
				else if (!client.getPersoon().getGeboortedatum().equals(geboortedatum))
				{
					error(getString("geen.met.bsn.en.geboortedatum"));
					return;
				}
				else
				{
					List<CervixUitnodiging> uitnodigingen = new ArrayList<>();
					if (client.getCervixDossier() != null && client.getCervixDossier().getLaatsteScreeningRonde() != null
						&& client.getCervixDossier().getLaatsteScreeningRonde().getUitnodigingen() != null)
					{

						for (CervixUitnodiging uitnodiging : client.getCervixDossier().getLaatsteScreeningRonde().getUitnodigingen())
						{
							if (uitnodiging.getMonster() != null)
							{
								uitnodigingen.add(uitnodiging);
							}
						}
					}
					toonUitnodigingen(target, client, uitnodigingen);
					return;
				}
			}
		};
		form.add(zoekenButton);
		form.setDefaultButton(zoekenButton);

		add(new AjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setUitnodiging(target, null);
			}
		});
	}

	private void toonUitnodigingen(AjaxRequestTarget target, Client client, List<CervixUitnodiging> uitnodigingen)
	{
		uitnodigingen.sort(Comparator.comparing(TablePerClassHibernateObject::getId));
		uitnodigingenPanel.replaceUitnodigingenPanel(target, uitnodigingen);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_UITNODIGINGEN_INGEZIEN, ScreenitSession.get().getLoggedInAccount(), client, getString("titel"),
			Bevolkingsonderzoek.CERVIX);
	}

	public abstract void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(bsnModel);
	}
}
