package nl.rivm.screenit.main.web.gebruiker.algemeen.retourzending;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.lang.reflect.ParameterizedType;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.RetourzendingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ZoekMetScannedInputPanel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.RetourredenAfhandeling;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.WrongClassException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_RETOURZENDINGEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class RetourzendingHandmatigVerwerkenPage extends RetourzendingBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(RetourzendingHandmatigVerwerkenPage.class);

	@SpringBean
	private RetourzendingService retourzendingService;

	@SpringBean
	private HibernateService hibernateService;

	private final WebMarkupContainer uitnodigingContainer;

	private final TransparentWebMarkupContainer fragments;

	private ZoekMetScannedInputPanel zoekPanel;

	public RetourzendingHandmatigVerwerkenPage()
	{

		fragments = new TransparentWebMarkupContainer("fragments");
		add(fragments);

		uitnodigingContainer = new WebMarkupContainer("uitnodigingContainer");
		uitnodigingContainer.setOutputMarkupPlaceholderTag(true);
		uitnodigingContainer.setVisible(false);
		uitnodigingContainer.addOrReplace(new EmptyPanel("fragmentContainer"));
		add(uitnodigingContainer);

		zoekPanel = new ZoekMetScannedInputPanel("scanUitnodiging")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void processScannedInput(AjaxRequestTarget target)
			{
				String uitnodigingId = getScanInput();
				if (StringUtils.isNotBlank(uitnodigingId) && StringUtils.isNumeric(uitnodigingId))
				{
					if (!new IFOBTUitnodigingsIdValidator().valideerUitnodiging(target, uitnodigingId)
						&& !new ZASUitnodigingsIdValidator().valideerUitnodiging(target, uitnodigingId))
					{
						error(getString("geen.uitnodiging.gevonden"));
					}
				}
				else
				{
					error(getString("geen.valide.barcode"));
				}
			}

		};

		add(zoekPanel);
	}

	protected abstract class UitnodigingsIdValidator<U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<?, ?, ?, ?>>
	{
		final boolean valideerUitnodiging(AjaxRequestTarget target, String uitnodigingId)
		{
			U uitnodiging = null;
			try
			{
				Map<String, Long> parameters = new HashMap<>();
				parameters.put("uitnodigingsId", Long.parseLong(uitnodigingId));
				Class<U> uitnodigingClass = (Class<U>) ((ParameterizedType) UitnodigingsIdValidator.this.getClass().getGenericSuperclass()).getActualTypeArguments()[0];
				uitnodiging = hibernateService.getUniqueByParameters(uitnodigingClass, parameters);
			}
			catch (WrongClassException e)
			{
				LOG.error("Fout bij valideren uitnodiging bij retourzending. uitnodigingsid: " + uitnodigingId, e);

			}
			if (uitnodiging != null && !isDossierInactiefOfRondeAfgerond(uitnodiging) && isUitnodigingValide(uitnodiging))
			{
				uitnodigingContainer.addOrReplace(new FormFragment<>("fragmentContainer", ModelUtil.sModel(uitnodiging)));
				uitnodigingContainer.setVisible(true);
				target.add(uitnodigingContainer);
			}
			return uitnodiging != null;
		}

		protected abstract boolean isUitnodigingValide(U uitnodiging);

		private boolean isDossierInactiefOfRondeAfgerond(U uitnodiging)
		{
			if (retourzendingService.isDossierInactiefOfRondeAfgerond(uitnodiging))
			{
				error(getString("geen.lopende.ronde"));
				return true;
			}
			else
			{
				return false;
			}
		}

	}

	protected class IFOBTUitnodigingsIdValidator extends UitnodigingsIdValidator<ColonUitnodiging, ColonScreeningRonde>
	{

		@Override
		protected boolean isUitnodigingValide(ColonUitnodiging uitnodiging)
		{
			String errorString = retourzendingService.isValideColonUitnodiging(uitnodiging);
			if (errorString == null)
			{
				return true;
			}
			else
			{
				error(getString(errorString));
			}
			return false;
		}
	}

	protected class ZASUitnodigingsIdValidator extends UitnodigingsIdValidator<CervixUitnodiging, CervixScreeningRonde>
	{

		@Override
		protected boolean isUitnodigingValide(CervixUitnodiging uitnodiging)
		{
			String errorString = retourzendingService.isValideCervixUitnodiging(uitnodiging);
			if (errorString == null)
			{
				return true;
			}
			else
			{
				error(getString(errorString));
			}
			return false;
		}
	}

	private class FormFragment<U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<?, ?, ?, ?>> extends Fragment
	{

		private static final long serialVersionUID = 1L;

		private IModel<U> uitnodingModel;

		public FormFragment(String id, IModel<U> model)
		{
			super(id, "formFragment", fragments, model);

			this.uitnodingModel = model;

			Form<U> statusForm = new Form<>("statusForm", new CompoundPropertyModel<>(model));
			add(statusForm);
			statusForm.add(new AjaxLink<Void>("opnieuw")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					reset(target);
				}
			});

			List<RetourredenAfhandeling> retourRedenAfhandelingen = hibernateService.loadAll(RetourredenAfhandeling.class);
			List<String> retourRedenen = retourRedenAfhandelingen.stream().map(r -> r.getRetourReden()).collect(Collectors.toList());

			statusForm.add(new ScreenitDropdown<String>("retourzendingReden", retourRedenen, new ChoiceRenderer<String>())
				.setNullValid(false).setRequired(true));

			statusForm.add(new IndicatingAjaxSubmitLink("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					U uitnodiging = uitnodingModel.getObject();
					retourzendingService.verwerkRetourzendingHandmatig(ScreenitSession.get().getLoggedInInstellingGebruiker(), uitnodiging, uitnodiging.getRetourzendingReden());

					reset(target);
					info(getString("message.gegevensopgeslagen"));
				}
			});

			statusForm.add(new AjaxLink<Void>("annuleren")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					reset(target);
				}
			});

			statusForm.add(new Label("monsterType", RetourzendingHandmatigVerwerkenPage.this.getString(model.getObject().getClass().getSimpleName() + ".type")));

		}

	}

	private void reset(AjaxRequestTarget target)
	{
		zoekPanel.reset(target);
		uitnodigingContainer.setVisible(false);
		uitnodigingContainer.addOrReplace(new EmptyPanel("fragmentContainer"));
		target.add(uitnodigingContainer);
	}

}
